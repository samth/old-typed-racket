;;; ==================================================================

#|

This solution is further optimized by looking up variable positions at
compile time, and using these at run-time.  See the code for comments.

This is an additional optimization that takes the previous one
further: an environment is now a list of frames, each one is a vector
of values.  No names, because they are not needed at run-time, and
using vectors because they have constant-time lookup.  Closures also
hold no names, just the closure's arity.  Additionally, the global
environment has turned to a global mapping of values.

You can try various versions with:

  (time (run "{bindrec {{fib {fun {n}
                               {if {< n 2}
                                 n
                                 {+ {fib {- n 1}}
                                    {fib {- n 2}}}}}}}
                {fib 25}}"))

-- click the "Show Details" in the language selection dialog and
choose "no debugging or profiling" to avoid measuring DrScheme's
debugging overhead.

On my computer I get 6.25 seconds for TOY (extended by HW#7), 3.9
seconds for compiled TOY (the basic solution for HW#8), 3.2 seconds
for the name optimization, and 2.65 seconds for this version.

|#

;;; ==================================================================
;;; Syntax

#| The BNF:
   <TOY> ::= <num>
           | <id>
           | { bind    {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { bindrec {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { fun  { <id> ... } <TOY> <TOY> ... }
           | { rfun { <id> ... } <TOY> <TOY> ... }
           | { if <TOY> <TOY> <TOY> }
           | { set! <id> <TOY> }
           | { <TOY> <TOY> ... }
|#

;; A matching abstract syntax tree datatype:
(define-type TOY
  [Num  (n number?)]
  [Id   (name symbol?)]
  [Bind (names unique-names?)
        (exprs (list-of TOY?))
        (body (list-of TOY?))]
  [BindRec (names unique-names?)
           (exprs (list-of TOY?))
           (body (list-of TOY?))]
  [Fun  (names unique-names?) (body (list-of TOY?))]
  [RFun (names unique-names?) (body (list-of TOY?))]
  [Call (fun-expr TOY?) (arg-exprs (list-of TOY?))]
  [If   (cond-expr TOY?) (then-expr TOY?) (else-expr TOY?)]
  [Set  (id symbol?) (expr TOY?)])

;; unique-list? : list -> bool
;; Tests whether a list is unique, used to make `unique-names?' below.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

;; unique-names? : any -> bool
;; A predicate that is used to specify a type of unique symbol lists.
(define unique-names?
  (intersection-of (list-of symbol?) unique-list?))

;; parse-sexpr : s-expr -> TOY
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons (or 'bind 'bindrec) more)
     (match sexpr
       [(list binder (list (list (symbol: names) nameds) ...) body)
        (let ([binder (if (eq? 'bind binder) Bind BindRec)])
          (binder names (map parse-sexpr nameds) (parse-sexpr body)))]
       [(cons binder more)
        (error 'parse-sexpr "bad `~s' syntax in ~s" binder sexpr)])]
    [(cons (or 'fun 'rfun) more)
     (match sexpr
       [(list funer (list (symbol: names) ...) body)
        (let ([funer (if (eq? 'fun funer) Fun RFun)])
          (funer names (parse-sexpr body)))]
       [(cons funer more)
        (error 'parse-sexpr "bad `~s' syntax in ~s" funer sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond) (parse-sexpr then) (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(cons 'set! more)
     (match sexpr
       [(list 'set! (symbol: name) value)
        (Set name (parse-sexpr value))]
       [else (error 'parse-sexpr "bad `set!' syntax in ~s" sexpr)])]
    [(list fun args ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; parse : string -> TOY
;; Parses a string containing an TOY expression to a TOY AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ==================================================================
;;; Values and environments

;; Not really defining this, but should be:
;;   ENV ::= (list-of (vector-of VAL))

;; Cheap hack to have ENV?
(define ENV? (list-of vector?))

(define-type VAL
  [BogusV] ; used for bindrec's temporary values
  [ScmV   (x any?)]
  ;; closures do not hold names, only the arity
  [FunV   (arity integer?) (body procedure?) (env ENV?)]
  [RFunV  (arity integer?) (body procedure?) (env ENV?)]
  [PrimV  (prim procedure?)])

;; optimize: a bogus value
(define bogus (BogusV))

;; extend-boxes : (list-of (box-of VAL)) ENV -> ENV
;; extends an environment with a new frame holding the given boxes.
;; note that this does not need names!
(define (extend-boxes boxes env)
  (cons (list->vector boxes) env))

;; extend : (list-of symbol) (list-of VAL) ENV -> ENV
;; extends an environment with a new frame (no names here either).
(define (extend values env)
  (extend-boxes (map box values) env))

;; extend-rec : (list-of (ENV -> VAL)) ENV -> ENV
;; extends an environment with a new recursive frame.
(define (extend-rec compiled-exprs env)
  (let* ([new-cells (map (lambda (x) (box bogus)) compiled-exprs)]
         [new-env (cons (list->vector new-cells) env)]
         [values (map (lambda (c) (c new-env)) compiled-exprs)])
    (for-each (lambda (cell val) (set-box! cell val))
              new-cells values)
    new-env))

;; find-variable-index : symbol (list-of (list-of symbol))
;;                       -> (union (list int int) false)
;; consumes a variable name and an environment description, and
;; returns the location of the variable in the environment as a pair
;; of the frame number (zero is the current frame, one is the
;; previous, etc) and the location within this frame; returns #f if
;; the variable is not found
(define (find-variable-index name bindings)
  ;; (note that in this function `env' & `frame' are descriptions, not
  ;; real environment pieces.)
  ;; helper for looking in the frame
  (define (find-in-frame frame n)
    (cond [(null? frame) #f]
          [(eq? name (car frame)) n]
          [else (find-in-frame (cdr frame) (+ n 1))]))
  ;; helper for searching for the right frame
  (define (find-in-env env n)
    (if (null? env)
      #f
      (let ([m (find-in-frame (car env) 0)])
        (if m
          (list n m)
          (find-in-env (cdr env) (+ n 1))))))
  ;; start the search
  (find-in-env bindings 0))
;; tests for this:
(test (find-variable-index 'x '((a b c) (a b x c) (x)))
      => '(1 2))
(test (find-variable-index 'y '((a b c) (a b x c) (x)))
      => #f)

;; lookup/idx : (list int int) ENV -> (box-of VAL)
;; similar to `lookup', but uses a variable index, as computed by the
;; above
(define (lookup/idx idx env)
  ;; note how there is no error checking -- we know that the variable
  ;; index is correct!
  (vector-ref (list-ref env (first idx)) (second idx)))

;; scheme-func->prim-val : (any ... -> any) -> VAL
;; converts a scheme function to a primitive evaluator function which
;; is a PrimV holding a ((list-of any) -> any) procedure.  (the result
;; procedure doesn't check for types and arity.)  No boxing is needed!
(define (scheme-func->prim-val scheme-func)
  (PrimV (lambda (args)
           (let ([args (map (lambda (a)
                              (cases a
                                [(ScmV v) v]
                                [else (error 'scheme-func
                                             "bad input: ~s" a)]))
                            args)])
             (ScmV (apply scheme-func args))))))

;; The global environment is really just a simple mapping now, no
;; frames, no boxing
(define global-mapping
  (list (list '+ (scheme-func->prim-val +))
        (list '- (scheme-func->prim-val -))
        (list '* (scheme-func->prim-val *))
        (list '/ (scheme-func->prim-val /))
        (list '< (scheme-func->prim-val <))
        (list '> (scheme-func->prim-val >))
        (list '= (scheme-func->prim-val =))
        (list 'list (scheme-func->prim-val list))
        ;; values
        (list 'true  (box (ScmV #t)))
        (list 'false (box (ScmV #f)))))

;;; ==================================================================
;;; Compilation

;; compiler-enabled? : (box-of bool)
;; a global flag that can disable the compiler
(define compiler-enabled? (box #f))

;; compile-body : (list-of TOY) (list-of (list-of symbol))
;;                -> (env -> VAL)
;; compiles a list of expressions, the result returns the last value.
(define (compile-body exprs bindings)
  (unless (unbox compiler-enabled?)
    (error 'compile "compiler disabled"))
  (if (null? exprs)
    (error 'compile-body "got an empty body")
    (let ([compiled-first (compile (first exprs) bindings)]
          [other-exprs    (rest exprs)])
      (if (null? other-exprs)
        compiled-first
        (let ([compiled-rest (compile-body other-exprs bindings)])
          (lambda (env)
            (compiled-first env)
            (compiled-rest env)))))))

;; compile-file: (int (ENV -> VAL) ENV -> VAL) (list-of symbol)
;;               (list-of TOY) (list-of (list-of symbol))
;;               -> VAL
;; A utility for the Fun and RFun cases of `compile'
(define (compile-fun make-closure names body bindings)
  (let ([compiled-body (compile-body body (cons names bindings))]
        ;; check unique names here (no longer checked by R/FunV)
        [arity (if (unique-names? names)
                 (length names)
                 (error 'compile-fun "expects unique names"))])
    (lambda (env)
      (make-closure arity compiled-body env))))

;; compile : TOY (list-of (list-of symbol)) -> (env -> VAL)
;; compiles TOY expressions.  The `bindings' argument is a list of
;; lists of names, which represents the names in the currently
;; compiled environment frames that we expect to be there.
(define (compile expr bindings)
  (define (compile* expr) (compile expr bindings)) ; a quick helper
  (unless (unbox compiler-enabled?)
    (error 'compile "compiler disabled"))
  (cases expr
    [(Num n)
     (let ([v (ScmV n)])
       (lambda (env) v))]
    [(Id v)
     (let ([idx (find-variable-index v bindings)])
       (if idx
         (lambda (env) (unbox (lookup/idx idx env)))
         ;; if it wasn't found, then it must be a global name; note
         ;; that there is no global environment -- just a mapping
         (let ([cell (assq v global-mapping)])
           (if cell
             (let ([val (second cell)])
               (lambda (env) val))
             (error 'compile "free variable name: ~s" name)))))]
    [(Set id expr)
     (let ([compiled-expr (compile* expr)]
           [idx (find-variable-index id bindings)])
       ;; cannot change globals, because there is no global
       ;; environment!
       (if idx
         (lambda (env)
           (set-box! (lookup/idx idx env) (compiled-expr env))
           bogus)
         (error 'compile
                "cannot mutate global or inexistent: ~s" id)))]
    [(Bind names exprs bound-body)
     (let ([compiled-exprs (map compile* exprs)] ; in the same scope
           ;; add another frame description to the bindings
           [compiled-body (compile-body bound-body
                                        (cons names bindings))])
       (lambda (env)
         (compiled-body
          (extend (map (lambda (c) (c env)) compiled-exprs) env))))]
    [(BindRec names exprs bound-body)
     (let* ([bindings (cons names bindings)]
            [compile* (lambda (e) (compile e bindings))]
            [compiled-exprs (map compile* exprs)]
            [compiled-body  (compile-body bound-body bindings)])
       (lambda (env)
         (compiled-body (extend-rec compiled-exprs env))))]
    [(Fun names bound-body)
     (compile-fun FunV names bound-body bindings)]
    [(RFun names bound-body)
     (compile-fun RFunV names bound-body bindings)]
    [(Call fun-expr arg-exprs)
     (let ([compiled-fun-expr (compile* fun-expr)]
           [args-num          (length arg-exprs)]
           [compiled-args     (map compile* arg-exprs)]
           ;; precomputed indexes for the boxes
           [id-arg-idxs
            (and (andmap Id? arg-exprs)
                 (map (lambda (e)
                        ;; these are always found, because the same
                        ;; expressions are compiled above
                        (find-variable-index (Id-name e) bindings))
                      arg-exprs))])
       (lambda (env)
         (let ([fval (compiled-fun-expr env)]
               ;; delay the evaluation of the arguments (use if
               ;; needed)
               [arg-vals (lambda ()
                           (map (lambda (c) (c env)) compiled-args))])
           (cases fval
             [(PrimV proc) (proc (arg-vals))]
             [(FunV arity compiled-body fun-env)
              ;; `extend' can no longer check for arity errors, so we
              ;; must do it here (note that we could just save the
              ;; arity in a closure structure)
              (if (= arity args-num)
                (compiled-body (extend (arg-vals) fun-env))
                (error 'call "arity mismatch in function call"))]
             [(RFunV arity compiled-body fun-env)
              (cond
                [(not (= arity args-num))
                 (error 'call "arity mismatch in rfunction call")]
                [id-arg-idxs
                 (let ([boxes (map (lambda (idx) (lookup/idx idx env))
                                   id-arg-idxs)])
                   (compiled-body (extend-boxes boxes fun-env)))]
                [else
                 (error 'eval
                        "ref-functions expect only identifiers")])]
             [else (error 'compile
                          "function call with a non-function: ~s"
                          fval)]))))]
    [(If cond-expr then-expr else-expr)
     (let ([compiled-cond-expr (compile* cond-expr)]
           [compiled-then-expr (compile* then-expr)]
           [compiled-else-expr (compile* else-expr)])
       (lambda (env)
         ((if (cases (compiled-cond-expr env)
                [(ScmV v) v] ; Scheme value => use as boolean
                [else #t])   ; other values are always true
            compiled-then-expr
            compiled-else-expr)
          env)))]))

;; run : string -> any
;; compiles and runs a TOY program contained in a string
(define (run str)
  (set-box! compiler-enabled? #t)
  ;; note that there is no global environment -- it's just a mapping
  ;; now, with values that are inlined
  (let ([compiled (compile (parse str) '())])
    (set-box! compiler-enabled? #f)
    (let ([result (compiled '())])
      (cases result
        [(ScmV v) v]
        [else (error 'run
                     "evaluation returned a bad value: ~s"
                     result)]))))

;;; ==================================================================
;;; Tests

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                   {add1 {fun {x} {+ x 1}}}}
              {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                   {foo {fun {x} {+ x 1}}}}
              {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
              {bind {{f {fun {y} {+ x y}}}}
                {bind {{x 5}}
                  {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
              {fun {x} {fun {y} {+ x y}}}}
             123}")
      => 124)
;; testing `set!' without multiple body expressions is tricky
(test (run "{bind {{x 123}}
              {bind {{y {set! x {+ x 1}}}}
                x}}")
      => 124)
;; test bindrec
(test (run "{bindrec {{fact {fun {n}
                              {if {= 0 n}
                                1
                                {* n {fact {- n 1}}}}}}}
              {fact 5}}")
      => 120)
;; test mutation and multiple body expressions
(test (run "{bind {{make-counter
                    {fun {}
                      {bind {{c 0}}
                        {fun {}
                          {set! c {+ 1 c}}
                          c}}}}}
              {bind {{c1 {make-counter}}
                     {c2 {make-counter}}}
                {* {c1} {c1} {c2} {c1}}}}")
      => 6)
;; test mutation with bindrec
(test (run "{bindrec {{foo {fun {}
                             {set! foo {fun {} 2}}
                             1}}}
              {+ {foo} {* 10 {foo}}}}")
      => 21)
;; test by-reference function calling
(test (run "{bind {{swap! {rfun {x y}
                            {bind {{tmp x}}
                              {set! x y}
                              {set! y tmp}}}}
                   {a 1}
                   {b 2}}
              {swap! a b}
              {+ a {* 10 b}}}")
      => 12)

;;; ==================================================================
