
#| BNF for the ALGAE language:
     <PROGRAM> ::= { program <FUN> ... }
     <FUN>     ::= { fun <id> { <id> } <ALGAE> }
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
               | True
               | False
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { call <id> <ALGAE> }
               | { quote <id> }
               | { vcall <ALGAE> <ALGAE> }
|#

;; ALGAE abstract syntax trees

#lang s-exp typed-scheme/lang/main           
           


(define-type PROGRAM
  [Funs (funs (list-of FUN))])

(define-type FUN
  [Fun (name Symbol) (arg Symbol) (body ALGAE)])

(define-type ALGAE
  [Num    (n Number)]
  [Bool   (b Boolean)]
  [Add    (args (list-of ALGAE))]
  [Mul    (args (list-of ALGAE))]
  [Sub    (fst ALGAE) (more (list-of ALGAE))]
  [Div    (fst ALGAE) (more (list-of ALGAE))]
  [Id     (name Symbol)]
  [With   (name Symbol) (named ALGAE) (body ALGAE)]
  [Less   (lhs ALGAE) (rhs ALGAE)]
  [Equal  (lhs ALGAE) (rhs ALGAE)]
  [LessEq (lhs ALGAE) (rhs ALGAE)]
  [If     (cond ALGAE) (then ALGAE) (else ALGAE)]
  [Call   (fun Symbol) (arg ALGAE)]
  [Quote  (name Symbol)]
  [VCall  (fun ALGAE) (arg ALGAE)])



;; parse-expr : s-expr -> ALGAE
;; parses an s-expression into an ALGAE abstract syntax tree
(define: (parse-expr [sexpr : Sexp]) : ALGAE
  (match sexpr
    [(number: n) (Num n)]
    ;; the constants must precede the Id case
    ['True  (Bool #t)]
    ['False (Bool #f)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-expr named) (parse-expr body))]
       [else (error 'parse-expr "bad `with' syntax")])]
    [(cons 'call more)
     (match sexpr
       [(list 'call (symbol: name) arg) (Call name (parse-expr arg))]
       [else (error 'parse-expr "bad `call' syntax")])]
    [(cons 'quote more)
     (match sexpr
       [(list 'quote (symbol: name)) (Quote name)]
       [else (error 'parse-expr "bad `quote' syntax")])]
    [(cons op args)
     ;; always need to parse the argument subexpressions, so this
     ;; matches over the same structure but with the *parsed* args
     ;; instead of parsing them in each case
     (match (cons op (map parse-expr args))
       [(list '+ (ALGAE: args) ...)      (Add args)]
       [(list '* (ALGAE: args) ...)      (Mul args)]
       [(list '- fst (ALGAE: args) ...)  (Sub fst args)]
       [(list '/ fst (ALGAE: args) ...)  (Div fst args)]
       [(list '<  fst snd)      (Less   fst snd)]
       [(list '=  fst snd)      (Equal  fst snd)]
       [(list '<= fst snd)      (LessEq fst snd)]
       [(list 'if fst snd thrd) (If fst snd thrd)]
       [(list 'vcall fst snd)   (VCall fst snd)]
       ;; use the original form in error messages
       [else (error 'parse-expr "bad form: ~s" sexpr)])]
    [else (error 'parse-expr "bad syntax in ~s" sexpr)]#||#))


;; parse-fun : s-expr -> FUN
;; parses a function s-expression syntax to an instance of FUN
(define: (parse-fun [sexpr : Sexp]) : FUN
  (match sexpr
    [(list 'fun (symbol: name) (list (symbol: arg)) body)
     (Fun name arg (parse-expr body))]
    [else (error 'parse-fun "bad function syntax: ~s" sexpr)]))

;; parse-program : s-expr -> PROGRAM
;; parses a whole program s-expression into a PROGRAM
(define: (parse-program [sexpr : Sexp]) : PROGRAM
  (match sexpr
    [(cons 'program funs) (Funs (map parse-fun funs))]
    [else (error 'parse-program "bad program syntax: ~s" sexpr)]))





;; parse : string -> PROGRAM
;; parses a string containing a ALGAE program to a PROGRAM instance
(define: (parse [str : String]) : PROGRAM
  (parse-program (string->sexpr str)))

;; subst : ALGAE symbol ALGAE -> ALGAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define: (subst [expr : ALGAE] [from : Symbol] [to : ALGAE]) : ALGAE
  ;; convenient helper -- no need to specify `from' and `to'
  (define: (subst* [x : ALGAE]) : ALGAE
    (subst x from to))
  ;; helper to substitute lists
  (define: (list-subst [exprs : (Listof ALGAE)]) : (Listof ALGAE)
    (map (lambda: ([x : ALGAE]) (subst x from to)) exprs))
  (cases expr
    [(Num n)         expr]
    [(Bool b)        expr]
    [(Quote name)    expr]
    [(Add args)     (Add (list-subst args))]
    [(Mul args)     (Mul (list-subst args))]
    [(Sub fst args) (Sub (subst* fst) (list-subst args))]
    [(Div fst args) (Div (subst* fst) (list-subst args))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
             bound-body
             (subst* bound-body)))]
    [(Less   l r)    (Less   (subst* l) (subst* r))]
    [(Equal  l r)    (Equal  (subst* l) (subst* r))]
    [(LessEq l r)    (LessEq (subst* l) (subst* r))]
    [(If c t e)      (If (subst* c) (subst* t) (subst* e))]
    [(Call fun arg)  (Call fun (subst* arg))]
    [(VCall fun arg) (VCall (subst* fun) (subst* arg))]))

;; lookup-fun : symbol PROGRAM -> FUN
;; looks up a FUN instance in a PROGRAM given its name
(define: (lookup-fun [name : Symbol] [prog : PROGRAM]) : FUN
  (cases prog
    [(Funs funs)
     (or (ormap (lambda: ([fun : FUN])
                  ;; `ormap' will return the first true (= non-#f)
                  ;; result, so this is both a predicate and returning
                  ;; the value that is used
                  (cases fun
                    [(Fun fname arg expr) (and (eq? fname name) fun)]))
                funs)
         (error 'lookup-fun
                "missing function definition for: ~s" name))]))

;; eval-number : ALGAE PROGRAM -> number
;; helper for `eval': verifies that the result is a number
(define: (eval-number [expr : ALGAE] [prog : PROGRAM]) : Number
  (let ([result (eval expr prog)])
    (if (number? result)
      result
      (error 'eval-number "need a number when evaluating ~s, but got ~s"
             expr result))))

;; eval-boolean : ALGAE PROGRAM -> boolean
;; helper for `eval': verifies that the result is a boolean
(define: (eval-boolean [expr : ALGAE] [prog : PROGRAM]) : Boolean
  (let ([result (eval expr prog)])
    (if (boolean? result)
      result
      (error 'eval-boolean "need a boolean when evaluating ~s, but got ~s"
             expr result))))

;; eval-symbol : ALGAE PROGRAM -> symbol
;; helper for `eval': verifies that the result is a symbol
(define: (eval-symbol [expr : ALGAE] [prog : PROGRAM]) : Symbol
  (let ([result (eval expr prog)])
    (if (symbol? result)
      result
      (error 'eval-symbol "need a symbol when evaluating ~s, but got ~s"
             expr result))))

;; value->algae : (union-of number boolean symbol) -> ALGAE
;; converts a value to an ALGAE value (so it can be used with `subst')
(define: (value->algae [val : (U Number Boolean Symbol)]) : ALGAE
  (cond [(number?  val) (Num val)]
        [(boolean? val) (Bool val)]
        [(symbol?  val) (Quote val)]
        #;
        [else (error 'value->algae "unexpected value: ~s" val)]))

;; test for an otherwise unreachable error:
;; error doesn't typecheck
;(test (value->algae null) =error> "unexpected value")
;; (do *not* do this for other helpers -- your tests should all go
;; through `eval')

;; eval : ALGAE PROGRAM -> (union-of number boolean symbol)
;; evaluates ALGAE expressions by reducing them to numbers
;; `prog' is provided for function lookup
(define: (eval [expr : ALGAE] [prog : PROGRAM]) : (U Number Boolean Symbol)
  (let ([eval (lambda: ([expr : ALGAE]) (eval expr prog))]
        [eval-number  (lambda: ([expr : ALGAE]) (eval-number  expr prog))]
        [eval-boolean (lambda: ([expr : ALGAE]) (eval-boolean expr prog))])
    ;; convenient helper
    (define: (fold-evals [f : (Number Number -> Number)] [init : Number] [exprs : (Listof ALGAE)]) : Number
      (foldl f init (map eval-number exprs)))
    (cases expr
      [(Num n) n]
      [(Bool b) b]
      [(Quote name) name]
      [(Add args) (fold-evals + 0 args)]
      [(Mul args) (fold-evals * 1 args)]
      [(Sub fst args)
       (let ([x (eval-number fst)]) ; need to evaluate in any case
         (if (null? args) (- x) (- x (fold-evals + 0 args))))]
      [(Div fst args)
       (let ([x (eval-number fst)]) ; need to evaluate in any case
         (if (null? args) (/ x) (/ x (fold-evals * 1 args))))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    ;; see the above `value->algae' helper
                    (value->algae (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Less   l r) (<  (eval-number l) (eval-number r))]
      [(Equal  l r) (=  (eval-number l) (eval-number r))]
      [(LessEq l r) (<= (eval-number l) (eval-number r))]
      [(If c t e) (eval (if (eval-boolean c) t e))]
      [(Call fun-name arg)
       (cases (lookup-fun fun-name prog)
         [(Fun name bound-id body)
          (eval (subst body bound-id (value->algae (eval arg))))])]
      [(VCall fun arg)
       (cases (lookup-fun (eval-symbol fun prog) prog)
         [(Fun name bound-id body)
          (eval (subst body bound-id (value->algae (eval arg))))])])))

;; run : string (union-of number boolean symbol)
;;       -> (union-of number boolean symbol)
;; evaluate an ALGAE complete program contained in a string using a
;; given value for the `main' function
(define: (run [str : String] [arg : (U Number Boolean Symbol)]) : (U Number Boolean Symbol)
  (let ([prog (parse str)])
    (eval (Call 'main (value->algae arg)) prog)))

;; run* : string -> (union-of number boolean symbol)
;; a version for testing simple ALGAE expressions without function calls
(define: (run* [str : String]) : (U Number Boolean Symbol)
  (eval (parse-expr (string->sexpr str)) (Funs null)))

;; tests (for simple expressions)
(test (run* "5") => 5)
(test (run* "{+ 5 5}") => 10)
(test (run* "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run* "{with {x 5} {+ x x}}") => 10)
(test (run* "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run* "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run* "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run* "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run* "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run* "{with {x 5} {with {y x} y}}") => 5)
(test (run* "{with {x 5} {with {x x} x}}") => 5)

;; additional tests for complete coverage (part 0)
(test (run* "x") =error> "free identifier")
(test (run* "{with {x 2} {/ 12 {* x 3}}}") => 2)
(test (run* "{with}") =error> "bad `with' syntax")
(test (run* "{foo}") =error> "bad form")
(test (run* "{}") =error> "bad syntax in")

;; test Scheme-like arithmetics
(test (run* "{+}") => 0)
(test (run* "{*}") => 1)
(test (run* "{+ 10}") => 10)
(test (run* "{* 10}") => 10)
(test (run* "{- 10}") => -10)
(test (run* "{/ 10}") => 1/10)
(test (run* "{+ 1 2 3 4}") => 10)
(test (run* "{* 1 2 3 4}") => 24)
(test (run* "{- 10 1 2 3 4}") => 0)
(test (run* "{/ 24 1 2 3 4}") => 1)

;; test boolean comparators and `if'
(test (run* "{< 1 2}"))
(test (not (run* "{= 1 2}")))
(test (run* "{if {<= 4 4} 5 6}") => 5)
(test (run* "{if True False 6}") => #f)
(test (run* "{+ {< 1 2}}") =error> "need a number")
(test (run* "{if 1 2 3}") =error> "need a boolean")
(test (run* "{with {b {<= 4 5}} {if b b b}}") => #t)
(test (run* "{with {x 5} {if {< x 5} {= x 4} {<= x 7}}}"))
(test (run* "{with {b {= 3 4}} {with {x 5} {if b x x}}}") => 5)

;; test a real program
(test (run "{program
              {fun even? {n}
                {if {= 0 n} True {call odd? {- n 1}}}}
              {fun odd? {n}
                {if {= 0 n} False {call even? {- n 1}}}}
              {fun main {n}
                {if {= n 1}
                  1
                  {+ 1 {call main
                             {if {call even? n}
                               {/ n 2}
                               {+ 1 {* n 3}}}}}}}}"
           3)
      => 8)

;; some test cases for full coverage
(test (run "1" 1)
      =error> "bad program syntax")
(test (run "{program 1}" 1)
      =error> "bad function syntax")
(test (run "{program {fun main {x} {call main}}}" 1)
      =error> "bad `call' syntax")
(test (run "{program {fun main {x} {call foo x}}}" 1)
      =error> "missing function definition")
(test (run "{program {fun main {x} {with {y 1} {+ x y}}}}" 1)
      => 2)
(test (run "{program {fun main {x} {with {foo 1} {call foo foo}}}
                     {fun foo {x} {- x -1}}}"
           1)
      => 2)
(test (run "{program
              {fun main {x}
                {*{+{*{+{*}{*}}{+{*}{*}{*}{*}}{+{*}{*}{*}{*}}}{*}}
                  {+{*}{*}{*}{*}{*}}
                  {+{*}{*}{*}{*}}}}}" 1)
      => 660)

;; test that the language is not higher order
(test (run "{program {fun foo {foo} foo}
                     {fun main {foo} {call foo foo}}}"
           1)
      => 1)

;; test the vcall facility
(test (run "{program
              {fun even? {n}
                {if {= 0 n} True {call odd? {- n 1}}}}
              {fun odd? {n}
                {if {= 0 n} False {call even? {- n 1}}}}
              {fun do_even {n}
                {/ n 2}}
              {fun do_odd {n}
                {+ 1 {* n 3}}}
              {fun main {n}
                {if {= n 1}
                  1
                  {+ 1 {call main
                             {vcall {if {call even? n}
                                      {quote do_even}
                                      {quote do_odd}}
                                    n}}}}}}"
           3)
      => 8)
;; more tests for complete coverage
(test (run "{program {fun main {n} {quote 1}}}" 0)
      =error> "bad `quote' syntax")
(test (run "{program {fun foo {n} {+ n 1}}
                     {fun main {n}
                       {with {proc {quote foo}}
                         {vcall proc n}}}}"
           3)
      => 4)
  
(test (run "{program {fun main {n} {vcall 3 4}}}" 1)
      =error> "need a symbol")

