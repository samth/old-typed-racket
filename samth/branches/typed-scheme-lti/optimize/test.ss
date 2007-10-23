;#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
;(begin
#;
(module test "../660-lang.ss" ;; .75 sec
     (define-type-alias u (U Symbol Number)))

;(module test "../660-lang.ss" ;; .75 sec

(require "../660-lang.ss")

(define-type-alias Sexp660 (mu x (U Symbol String Number Boolean (Listof x)))) ;; .04


(define-type ALGAE ;; .08
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



(define: (parse-expr [sexpr : Sexp660]) : ALGAE ;; .7 sec
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
       [(list-rest '+  args)      (Add args)]
       [(list-rest '* args)       (Mul args)]
       [(list-rest '- fst args)   (Sub fst args)]
       [(list-rest '/ fst args)   (Div fst args)]
       [(list '<  fst snd)        (Less   fst snd)]
       [(list '=  fst snd)        (Equal  fst snd)]
       [(list '<= fst snd)        (LessEq fst snd)]
       [(list 'if fst snd thrd)   (If fst snd thrd)]
       [(list 'vcall fst snd)     (VCall fst snd)]
       ;; use the original form in error messages
       [else (error 'parse-expr "bad form: ~s" sexpr)])]
    [else (error 'parse-expr "bad syntax in ~s" sexpr)]))

;)