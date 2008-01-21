#lang s-exp "../660/lang/main.ss"
;;; ==================================================================
;;; Syntax

#| The BNF:
   <SLUG> ::= <num>
            | <id>
            | { bind {{ <id> <SLUG> } ... } <SLUG> }
            | { with-stx {<id> { <id> ... }
                               { <pattern> <pattern> } ...}
                  <SLUG> }
            | { fun { <id> ... } <SLUG> }
            | { if <SLUG> <SLUG> <SLUG> }
            | { <SLUG> <SLUG> ... }
|#

;; A matching abstract syntax tree datatype:
;;   (note: no WithStx constructor -- it is preprocessed away)
(define-type SLUG
  [Num  (n Number)]
  [Str  (s String)]
  [Id   (name Symbol)]
  [Bind (names Names) (exprs (list-of SLUG)) (body SLUG)]
  [Fun  (names Names) (body SLUG)]
  [Call (fun-expr SLUG) (arg-exprs (list-of SLUG))]
  [If   (cond-expr SLUG) (then-expr SLUG) (else-expr SLUG)])

(define-type-alias Names (Listof Symbol))
#|
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
|#

#|
;; This built-in is used in the following code:
;; make-transformer : (list-of symbol) (list-of (list sexpr sexpr))
;;                    -> (sexpr -> sexpr)
;; consumes a list of pattern pairs, and creates a transformer
;; procedure (transforms an s-expression into an s-expression)

;; parse-sexpr : s-expr (list-of (list symbol (syntax -> syntax)))
;;               -> SLUG
;; parses *and* macro-expands an s-expression; the second argument is
;; the association list of transformers at this point.
(define (parse-sexpr sexpr transformers)
  ;; convenient function for common cases where we recurse with the
  ;; same transformers
  (define (parse* sexpr) (parse-sexpr sexpr transformers))
  (let ([transformer (and (pair? sexpr)
                          (assq (car sexpr) transformers))])
    (if transformer
      ;; if there is a transformer by this name, apply it and
      ;; continue with the result
      (parse* ((second transformer) sexpr))
      (match sexpr
        ;; if we see `with-stx', then recursively parse with an
        ;; additional transformer
        [(cons 'with-stx more)
         (match sexpr
           [(list 'with-stx
                  (list (symbol: name)
                        (list (symbol: keywords) ...)
                        more ...)
                  body)
            (parse-sexpr
             body
             (cons (list name (make-transformer keywords more))
                   transformers))]
           [else (error 'parse-sexpr
                        "bad `with-stx' syntax in ~s" sexpr)])]
        [(number: n)    (Num n)]
        [(symbol: name) (Id name)]
        [(string: s)    (Str s)]
        [(cons 'bind more)
         (match sexpr
           [(list 'bind (list (list (symbol: names) nameds) ...) body)
            (Bind names (map parse* nameds) (parse* body))]
           [else (error 'parse-sexpr "bad `bind' syntax in ~s" sexpr)])]
        [(cons 'fun more)
         (match sexpr
           [(list 'fun (list (symbol: names) ...) body)
            (Fun names (parse* body))]
           [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
        [(cons 'if more)
         (match sexpr
           [(list 'if cond then else)
            (If (parse* cond) (parse* then) (parse* else))]
           [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
        [(list fun args ...) ; other lists are applications
         (Call (parse* fun) (map parse* args))]
        [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))))

;; parse : string -> SLUG
;; Parses a string containing an SLUG expression to a SLUG AST.
(define (parse str)
  (parse-sexpr (string->sexpr str) global-transformers))
|#

;;; ==================================================================
;;; Values and environments

(define-type ENV
  [EmptyEnv]
  [FrameEnv (frame Frame) (rest ENV)])

(define-type VAL
  [ScmV  (x Any)]
  [IOV   (x IO)]
  [FunV  (names Names) (body SLUG) (env ENV)]
  [ExprV (expr SLUG)
         (env ENV)
         (cache (Box (U #f VAL)))]
  [PrimV (prim ((Listof VAL) -> VAL))])

;; I/O descriptions
(define-type IO
  [Print    (string VAL)]
  [ReadLine (receiver VAL)]
  [Begin2   (l VAL) (r VAL)]
  ;; mutation
  [NewBox   (init VAL)  (receiver VAL)]
  [UnBox    (boxed VAL) (receiver VAL)]
  [SetBox   (boxed VAL) (newval VAL)])

(define-type-alias Frame (Listof (Tuple Symbol VAL)))

#|
;; a frame is an association list of names and values.
(define frame?
  (list-of (lambda (x)
             (and (list? x)
                  (= 2 (length x))
                  (symbol? (first x))
                  (VAL? (second x))))))
|#


;; extend : (list-of symbol) (list-of VAL) ENV -> ENV
;; extends an environment with a new frame.
(define: (extend [names : Names] [values : (Listof VAL)] [env : ENV]) : ENV
  (if (= (length names) (length values))
    (FrameEnv (map (lambda: ([a : Symbol] [b : VAL]) (list a b)) names values) env)
    (error 'extend "arity mismatch for names: ~s" names)))

;; lookup : symbol ENV -> VAL
;; looks a name in an environment, searching through each frame.
(define: (lookup [name : Symbol] [env : ENV]) : VAL
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(FrameEnv frame rest)
     (let ([cell (assq name frame)])
       (if cell
         (second cell)
         (lookup name rest)))]))
#|
;; scheme-func->prim : (any ... -> any) (any -> VAL) bool -> VAL
;; Converts a scheme function to a primitive evaluator function which
;; is a PrimV holding a ((list-of SLUG-val) -> SLUG-val) procedure.
;; The `wrapper' argument is used to convert the result to a VAL.
;; (The result procedure doesn't check for types and arity.)
(define (scheme-func->prim scheme-func wrapper strict?)
  (PrimV (lambda (args)
           (let* ([args (if strict?
                          (map (lambda (a)
                                 (let ([v (strict a)])
                                   (cases v
                                     [(ScmV v) v]
                                     [else (error 'scheme-func
                                                  "bad input: ~s"
                                                  a)])))
                               args)
                          args)]
                  [result (apply scheme-func args)])
             ;; Because there are non-strict constructors, primitives
             ;; like `car' might be returning promises which are
             ;; already VAL objects.
             (if (VAL? result) result (wrapper result))))))

;; scheme-func->prim-val : (any ... -> any) bool -> VAL
;; Uses `scheme-func->prim' to return a ScmV value
(define (scheme-func->prim-val scheme-func strict?)
  (scheme-func->prim scheme-func ScmV strict?))

;; scheme-func->prim-io : (any ... -> any) bool -> VAL
;; Uses `scheme-func->prim' to return an IOV value
(define (scheme-func->prim-io scheme-func strict?)
  (scheme-func->prim scheme-func IOV strict?))
|#
;;; ==================================================================
;;; The global environment

(define global-environment
  (FrameEnv (list #|(list '+ (scheme-func->prim-val + #t))
                  (list '- (scheme-func->prim-val - #t))
                  (list '* (scheme-func->prim-val * #t))
                  (list '/ (scheme-func->prim-val / #t))
                  (list '< (scheme-func->prim-val < #t))
                  (list '> (scheme-func->prim-val > #t))
                  (list '= (scheme-func->prim-val = #t))
                  (list 'number->string
                        (scheme-func->prim-val number->string #t))
                  ;; Note flags:
                  (list 'cons  (scheme-func->prim-val cons* #f))
                  (list 'list  (scheme-func->prim-val list  #f))
                  (list 'car   (scheme-func->prim-val car   #t))
                  (list 'cdr   (scheme-func->prim-val cdr   #t))
                  (list 'null? (scheme-func->prim-val null? #f))
                  ;; IO constructors -- all are non-strict
                  (list 'print  (scheme-func->prim-io Print    #f))
                  (list 'read   (scheme-func->prim-io ReadLine #f))
                  (list 'begin2 (scheme-func->prim-io Begin2   #f))
                  ;; Mutation constructors -- similar to the above
                  (list 'newbox   (scheme-func->prim-io NewBox #f))
                  (list 'unbox    (scheme-func->prim-io UnBox  #f))
                  (list 'set-box! (scheme-func->prim-io SetBox #f))
                  |#
                  ;; Values
                  (list 'true  (ScmV #t))
                  (list 'false (ScmV #f))
                  (list 'null  (ScmV null)))
            (EmptyEnv)))
#|
;;; ==================================================================
;;; The global transformers

;; This is a utility for conveniently constructing the
;; global-transformers list.
;; Reminders:
;; * the transformers are a (list-of (list symbol (syntax -> syntax)))
;; * and we have
;;   make-transformer : (list-of symbol) (list-of (list sexpr sexpr))
;;                      -> (sexpr -> sexpr)

;; named-transformers : symbol (list-of symbol) (list-of string ...)
;;                      -> (list symbol (syntax -> syntax))
;; constructs a named transformer (which is one element in the
;; global-transformers) with the given name from the given list of
;; keywords and an a list with an *even* number of SLUG strings, where
;; each pair is an input/output template pair (these strings are
;; parsed to get the template s-expression)
(define (named-transformer name keywords templates)
  ;; simple version, could be improved by making it tail recursive
  (define (even-list->list-of-2lists l)
    (cond [(null? l) '()]
          [(null? (cdr l))
           (error 'named-transformer "odd number of templates")]
          [else (cons (list (first l) (second l))
                      (even-list->list-of-2lists (cddr l)))]))
  (list name (make-transformer keywords
                               (even-list->list-of-2lists
                                (map string->sexpr templates)))))

(define global-transformers
  (list (named-transformer 'let '()
          '("{let {{var val} ...} body}" "{{fun {var ...} body} val ...}"))
        (named-transformer 'let* '()
          '("{let* {} body}" "body"
            "{let* {{id1 expr1} {id expr} ...} body}" ; ->
              "{let {{id1 expr1}} {let* {{id expr} ...} body}}"))
        (named-transformer 'do '(<-)
          '("{do {id <- {f x ...}} next more ...}" ; ->
            "{f x ... {fun {id} {do next more ...}}}"
            "{do {f x ...} next more ...}" ; ->
              "{begin2 {f x ...} {do next more ...}}"
            "{do expr}" "expr"))
        (named-transformer 'prog '(:=)
          '("{prog {f x ...} := body more ...}" ; ->
              "{bind {{f {fun {x ...} body}}} {prog more ...}}"
            "{prog v := x more ...}" ; ->
              "{bind {{v x}} {prog more ...}}"
            "{prog expr}" ; ->
              "expr"))))
|#
;;; ==================================================================
;;; Evaluation

;; strict : VAL -> VAL which is not an ExprV
;; forces a (possibly nested) ExprV promise
(define: (strict [v : VAL]) : VAL
  (cases v
    [(ExprV expr env cache)
     (or (unbox cache)
         (let ([val (strict (eval expr env))])
           (set-box! cache val)
           val))]
    [else v]))

;; eval-promise : SLUG env -> VAL (the ExprV variant)
;; used instead of `eval' to create an evaluation promise
(define: (eval-promise [expr : SLUG] [env : ENV]) : VAL
  (ExprV expr env (box (ann #f : (Un #f VAL)))))

;; eval : SLUG env -> VAL
;; evaluates SLUG expressions.
(define: (eval [expr : SLUG] [env : ENV]) : VAL
  (cases expr
    [(Num n) (ScmV n)]
    [(Str s) (ScmV s)]
    [(Id name) (lookup name env)]
    [(Bind names exprs bound-body)
     (eval bound-body
           (extend names
                   (map (lambda: ([e : SLUG]) (eval-promise e env)) exprs)
                   env))]
    [(Fun names bound-body)
     (FunV names bound-body env)]
    [(Call fun-expr arg-exprs)
     (let ([fval (strict (eval fun-expr env))]
           [arg-vals (map (lambda: ([e : SLUG]) (eval-promise e env))
                          arg-exprs)])
       (cases fval
         [(PrimV proc) (proc arg-vals)]
         [(FunV names body fun-env)
          (eval body (extend names arg-vals fun-env))]
         [else (error 'eval "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (eval (if (cases (strict (eval cond-expr env))
                 [(ScmV v) v] ; Scheme value => use as boolean
                 [else #t])   ; other values are always true
             then-expr
             else-expr)
           env)]))

;;; ==================================================================
;;; I/O execution

;; force-io : IO -> IO
;; forces VAL objects in an IO value.
(define: (strict-IO [v : IO]) : IO
  (cases v
    [(Print    x) (Print    (strict x))]
    [(ReadLine x) (ReadLine (strict x))]
    [(Begin2 x y) (Begin2   (strict x) (strict y))]
    [(NewBox x y) (NewBox   (strict x) (strict y))]
    [(UnBox  x y) (UnBox    (strict x) (strict y))]
    [(SetBox x y) (SetBox   (strict x) (strict y))]))

;; execute-receiver : VAL any -> void
;; Helper for perform-i/o: this function expects a receiver value and
;; a return value to throw onto the receiver (in a ScmV wrapper).
(define: (execute-receiver [receiver-val : VAL] [return-val : Any]) : Void
  (cases receiver-val
    [(FunV names body env)
     (if (= 1 (length names))
       (execute
        (eval body (extend names (list (ScmV return-val)) env)))
       (error 'execute-receiver "expecting a unary function"))]
    [else (error 'execute-receiver "expecting a receiver function")]))

;; perform-i/o : IO -> void
;; Executes the described I/O operation
(define: (perform-i/o [v : IO]) : Void
  (let ([forced (strict-IO v)])
    (cases forced
      [(Print (ScmV str))
       (if (string? str)
         (display str)
         (error 'perform-i/o
                "cannot print a non-string value: ~s" str))]
      [(ReadLine receiver)
       (execute-receiver receiver (read-line))]
      [(Begin2 (IOV io1) (IOV io2))
       (perform-i/o io1)
       (perform-i/o io2)]
      ;; Mutations: always have a plain Scheme value in the boxes
      [(NewBox (ScmV init) receiver)
       (execute-receiver receiver (box init))]
      [(UnBox (ScmV the-box) receiver)
       (if (box? the-box)
         (execute-receiver receiver (unbox the-box))
         (error 'perform-i/o
                "cannot unbox a non-box value: ~s" the-box))]
      [(SetBox (ScmV the-box) (ScmV newval))
       (if (box? the-box)
         (set-box! the-box newval)
         (error 'perform-i/o
                "cannot set-box a non-box value: ~s" the-box))]
      [else (error 'perform-i/o "bad input: ~s" forced)])))

;; execute : VAL -> void
;; executes an IOV in a VAL
(define: (execute [val : VAL]) : Void
  (let ([val (strict val)])
    (cases val
      [(IOV v) (perform-i/o v)]
      [else (error 'execute "expecting an IO value: ~s" val)])))


#|
;;; ==================================================================
;;; Main entry points

;; run : string -> (union-of number string)
;; evaluate a SLUG program contained in a string
(define (run str)
  (let ([result (strict (eval (parse str) global-environment))])
    (cases result
      [(ScmV v) v]
      [else (error 'run
                   "evaluation returned a bad value: ~s" result)])))

;; run-io : string -> void
;; evaluate a SLUG program contained in a string, and execute the
;; resulting IOV description
(define (run-io str)
  (execute (eval (parse str) global-environment)))

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

;; test laziness
(test (run "{bind {{x {/ 1 0}}} {car {cons 1 null}}}")
      => 1)

;; test two macros
(test (run "{with-stx {let {}
                           {{let {{var val} ...} body}
                            {{fun {var ...} body} val ...}}}
              {with-stx {let* {}
                              {{let* {} body} body}
                              {{let* {{id1 expr1} {id expr} ...} body}
                               {let {{id1 expr1}}
                                 {let* {{id expr} ...}
                                   body}}}}
                {let* {{x 1} {y {+ x 1}}} {+ x y}}}}")
      => 3)

;; test simple IO
(run-io
 "{begin2 {print 'What is your name?\n'}
          {read {fun {name}
                  {begin2 {print 'Your name is '''}
                          {begin2 {print name}
                                  {print '''\n'}}}}}}")

;;; ==================================================================

;; IO with convenient macros -- solution to the last question
(run-io
 "{with-stx {do {<-}
                {{do {id <- {read}} next more ...}
                 {read {fun {id} {do next more ...}}}}
                {{do {print str} next more ...}
                 {begin2 {print str} {do next more ...}}}
                {{do expr}
                 expr}}
    {do {print 'What is your name?\n'}
        {name <- {read}}
        {print 'What is your email?\n'}
        {email <- {read}}
        {print 'Your address is '''}
        {print name}
        {print ' <'}
        {print email}
        {print '>''\n'}}}")

;;; ==================================================================

;; Test mutation
(run-io
 "{bind {{incbox {fun {b}
                   {unbox b
                     {fun {curval}
                       {set-box! b {+ 1 curval}}}}}}}
    {newbox 0
      {fun {i}
        {begin2
          {incbox i}
          {begin2
            {print 'i now holds: '}
            {unbox i
              {fun {v}
                {begin2 {print {number->string v}}
                        {print '\n'}}}}}}}}}")

;; Same as above, but with conveniet syntax
(run-io
 "{with-stx {do {<-}
                {{do {id <- {f x ...}} next more ...}
                 {f x ... {fun {id} {do next more ...}}}}
                {{do {f x ...} next more ...}
                 {begin2 {f x ...} {do next more ...}}}
                {{do expr}
                 expr}}
    {bind {{incbox {fun {b}
                     {do {curval <- {unbox b}}
                         {set-box! b {+ 1 curval}}}}}}
      {do {i <- {newbox 0}}
          {incbox i}
          {print 'i now holds: '}
          {v <- {unbox i}}
          {print {number->string v}}
          {print '\n'}}}}")

;;; ==================================================================

#| Bonus question #1:

   The problem is that make-transformer creates an s-expression
   transformation procedure.  Doing this over already-parsed concrete
   syntax will not make much sense, since part of this syntax may need
   to be modified as a result of applying macros.  This means that it
   is easier to expand macros over the input s-expression format, and
   adding a `syntax environment' is easy to slap on `parse'.  (We're
   basically using s-expressions as a convenient *uniform*
   representation.)

|#

#| Bonus question #2:
   See the above definition of global-transformers.  These
   transformers were not used in the above examples since they were
   always overwritten by a local `with-stx'.  Now, here are some
   examples that show how these definitions work:
|#

;; simple example of using `prog'
(run "{prog x := 1
            y := 2
            {foo n} := {+ n x}
            x := {+ x 1}
        {* x {foo y}}}")

;; similar to the above
(run-io
 "{prog
    {twice b} := {do {curval <- {unbox b}}
                   {set-box! b {* 2 curval}}}
    {do {i <- {newbox 1}}
        {twice i}
        {print 'i now holds: '}
        {v <- {unbox i}}
        {print {number->string v}}
        {twice i}
        {print ', and now it holds: '}
        {v <- {unbox i}}
        {print {number->string v}}
        {print '\n'}}}")

;;; ==================================================================
|#