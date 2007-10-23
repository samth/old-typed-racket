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
       [(list binder (list (list (symbol: names) nameds) ...) body ...)
        (let ([binder (if (eq? 'bind binder) Bind BindRec)])
          (binder names
                  (map parse-sexpr nameds)
                  (map parse-sexpr body)))]
       [(cons binder more)
        (error 'parse-sexpr "bad `~s' syntax in ~s" binder sexpr)])]
    [(cons (or 'fun 'rfun) more)
     (match sexpr
       [(list funer (list (symbol: names) ...) body ...)
        (let ([funer (if (eq? 'fun funer) Fun RFun)])
          (funer names (map parse-sexpr body)))]
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

(define-type ENV
  [EmptyEnv]
  [FrameEnv (frame frame?) (rest ENV?)])

(define-type VAL
  [BogusV] ; used for bindrec's temporary values
  [ScmV   (x any?)]
  [FunV   (names unique-names?) (body (list-of TOY?)) (env ENV?)]
  [RFunV  (names unique-names?) (body (list-of TOY?)) (env ENV?)]
  [PrimV  (prim procedure?)])

;; optimize: a bogus value
(define bogus (BogusV))

;; a frame is an association list of names and values.
(define frame?
  (list-of (lambda (x)
             (and (list? x)
                  (= 2 (length x))
                  (symbol? (first x))
                  ((box-of VAL?) (second x))))))

;; extend-boxes : (list-of symbol) (list-of (box-of VAL)) ENV -> ENV
;; extends an environment with a new frame holding the given boxes.
(define (extend-boxes names boxes env)
  (if (= (length names) (length boxes))
    (FrameEnv (map list names boxes) env)
    ;; this error can be caused by a call to `extend' too
    (error 'extend "arity mismatch for names: ~s" names)))

;; extend : (list-of symbol) (list-of VAL) ENV -> ENV
;; extends an environment with a new frame.
(define (extend names values env)
  (extend-boxes names (map box values) env))

;; extend-rec : (list-of symbol) (list-of TOY) ENV -> ENV
;; extends an environment with a new recursive frame.
(define (extend-rec names exprs env)
  (if (= (length names) (length exprs))
    (let* ([new-cells (map (lambda (x) (box bogus)) exprs)]
           [new-env (FrameEnv (map list names new-cells) env)]
           [values (map (lambda (e) (eval e new-env)) exprs)])
      (for-each (lambda (cell val) (set-box! cell val))
                new-cells values)
      new-env)
    (error 'extend-rec "arity mismatch for names: ~s" names)))
#| a different approach:
(define (extend-rec names exprs env)
  (if (= (length names) (length exprs))
    (let ([new-env (extend names (map (lambda (x) bogus) exprs) env)])
      (for-each (lambda (name expr)
                  (set-box! (lookup name new-env)
                            (eval expr new-env)))
                names exprs)
      new-env)
    (error 'extend-rec "arity mismatch for names: ~s" names)))
|#

;; lookup : symbol ENV -> (box-of VAL)
;; looks a name in an environment, searching through each frame.
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(FrameEnv frame rest)
     (let ([cell (assq name frame)])
       (if cell
         (second cell)
         (lookup name rest)))]))

;; scheme-func->prim-val : (any ... -> any) -> (box-of VAL)
;; converts a scheme function to a primitive evaluator function which
;; is a PrimV holding a ((list-of any) -> any) procedure.  (the result
;; procedure doesn't check for types and arity.)
(define (scheme-func->prim-val scheme-func)
  (box (PrimV
        (lambda (args)
          (let ([args (map (lambda (a)
                             (cases a
                               [(ScmV v) v]
                               [else (error 'scheme-func
                                            "bad input: ~s" a)]))
                           args)])
            (ScmV (apply scheme-func args)))))))

;; The global environment has a few primitives:
(define global-environment
  (FrameEnv (list (list '+ (scheme-func->prim-val +))
                  (list '- (scheme-func->prim-val -))
                  (list '* (scheme-func->prim-val *))
                  (list '/ (scheme-func->prim-val /))
                  (list '< (scheme-func->prim-val <))
                  (list '> (scheme-func->prim-val >))
                  (list '= (scheme-func->prim-val =))
                  (list 'list (scheme-func->prim-val list))
                  ;; values
                  (list 'true  (box (ScmV #t)))
                  (list 'false (box (ScmV #f))))
            (EmptyEnv)))

;;; ==================================================================
;;; Evaluation

;; eval-body : (list-of TOY) env -> VAL
;; evaluates a list of expressions, return the last value.
(define (eval-body exprs env)
  (if (null? exprs)
    (error 'eval-body "got an empty body")
    (let ([1st-value   (eval (first exprs) env)]
          [other-exprs (rest exprs)])
      (if (null? other-exprs)
        1st-value
        (eval-body other-exprs env)))))

;; eval : TOY env -> VAL
;; evaluates TOY expressions.
(define (eval expr env)
  (cases expr
    [(Num n) (ScmV n)]
    [(Id v) (unbox (lookup v env))]
    [(Bind names exprs bound-body)
     (eval-body bound-body
                (extend names
                        (map (lambda (e) (eval e env)) exprs)
                        env))]
    [(BindRec names exprs bound-body)
     (eval-body bound-body (extend-rec names exprs env))]
    [(Fun names bound-body)
     (FunV names bound-body env)]
    [(RFun names bound-body)
     (RFunV names bound-body env)]
    [(Call fun-expr arg-exprs)
     (let ([fval (eval fun-expr env)]
           ;; delay the evaluation of the arguments (use if needed)
           [arg-vals (lambda ()
                       (map (lambda (e) (eval e env)) arg-exprs))])
       (cases fval
         [(PrimV proc) (proc (arg-vals))]
         [(FunV names body fun-env)
          (eval-body body (extend names (arg-vals) fun-env))]
         [(RFunV names body fun-env)
          (if (andmap Id? arg-exprs)
            (let ([boxes (map (lambda (id) (lookup (Id-name id) env))
                              arg-exprs)])
              (eval-body body (extend-boxes names boxes fun-env)))
            (error 'eval "ref-functions expect only identifiers"))]
         [else (error 'eval "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (eval (if (cases (eval cond-expr env)
                 [(ScmV v) v] ; Scheme value => use as boolean
                 [else #t])   ; other values are always true
             then-expr
             else-expr)
           env)]
    [(Set id expr)
     ;; note the use of two expressions in this branch
     (set-box! (lookup id env) (eval expr env))
     bogus]))

;; run : string -> any
;; evaluate a TOY program contained in a string
(define (run str)
  (let ([result (eval (parse str) global-environment)])
    (cases result
      [(ScmV v) v]
      [else (error 'run
                   "evaluation returned a bad value: ~s" result)])))

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

;; More tests for complete coverage
(test (run "{bind 1 2}") =error> "bad `bind' syntax")
(test (run "{fun 1 2}")  =error> "bad `fun' syntax")
(test (run "{if 1 2}")   =error> "bad `if' syntax")
(test (run "{set! 1 2}") =error> "bad `set!' syntax")
(test (run "{}")         =error> "bad syntax")
(test (run "x")          =error> "no binding for x")
(test (run "{bind {}}")  =error> "got an empty body")
(test (run "{1 2}")      =error> "function call with a non-function")
(test (run "{+ {fun {x} x} 1}") =error> "bad input")
(test (run "{fun {x} x}") =error> "evaluation returned a bad value")
(test (run "{{rfun {x} x} 1}") =error> "ref-functions expect only")
(test (run "{bind {{x 1}} {{rfun {y} y}}}") =error> "arity mismatch")
(test (run "{if {fun {x} x} 1 2}") => 1)

;; can't test these through `run', because `parse' always returns the
;; same number of names and expressions for a `bindrec'
(test (extend-rec '(x) '() (EmptyEnv)) =error> "arity mismatch for")

;;; ==================================================================
