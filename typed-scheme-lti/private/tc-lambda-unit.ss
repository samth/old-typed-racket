(module tc-lambda-unit (lib "a-unit.ss")
  
  (require "planet-requires.ss"
           "signatures.ss"
           (lib "trace.ss")
           (all-except "type-rep.ss" make-arr) ;; doesn't need tests
           "type-effect-convenience.ss" ;; maybe needs tests
           "type-environments.ss" ;; doesn't need tests
           "lexical-env.ss" ;; maybe needs tests
           "type-annotation.ss" ;; has tests
           "utils.ss"
           "effect-rep.ss"
           "tc-utils.ss"
           (lib "plt-match.ss")
           (rename "type-effect-convenience.ss" make-arr make-arr*))
  (require-for-template mzscheme "internal-forms.ss")
  
  (require-galore)
  
  (import typechecker^)
  (export tc-lambda^)
  
  (define (remove-var id thns elss)
    (let/ec exit
      (define (fail) (exit #f))
      (define (rv e)
        (match e
          [(Var-True-Effect: v) (if (module-identifier=? v id) (make-Latent-Var-True-Effect) (fail))]
          [(Var-False-Effect: v) (if (module-identifier=? v id) (make-Latent-Var-False-Effect) (fail))]
          [(or (True-Effect:) (False-Effect:)) e]
          [(Restrict-Effect: t v) (if (module-identifier=? v id) (make-Latent-Restrict-Effect t) (fail))]
          [(Remove-Effect: t v) (if (module-identifier=? v id) (make-Latent-Remove-Effect t) (fail))]))
      (cons (map rv thns) (map rv elss))))
    
  
  ;; typecheck a single lambda, with argument list and body
  ;; fixme: abstract the two cases!
  ;; syntax-list expr-list -> type
  (define (tc/lambda-clause/check args body arg-tys ret-ty rest-ty)
    (syntax-case args ()
      [(args* ...)    
       (if (ormap (lambda (e) (not (type-annotation e))) (syntax->list #'(args* ...)))
           (let* ([arg-list (syntax->list #'(args* ...))])
             (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
             (with-lexical-env/extend 
              arg-list arg-tys
              (match (tc-exprs/check (syntax->list body) ret-ty)
                [(tc-result: t thn els)
                 (cond
                   ;; this is T-AbsPred                
                   ;; if this function takes only one argument, and all the effects are about that one argument
                   [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
                    => (lambda (thn/els) (make-arr arg-tys t #f (car thn/els) (cdr thn/els)))]
                   ;; otherwise, the simple case
                   [else (make-arr arg-tys t)])]
                [_ (int-err "bad match")])))
           (let* ([arg-list (syntax->list #'(args* ...))]
                  [arg-types (map get-type arg-list)])
             (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
             (with-lexical-env/extend 
              arg-list arg-types
              (match (tc-exprs/check (syntax->list body) ret-ty)
                [(tc-result: t thn els)
                 (cond
                   ;; this is T-AbsPred                
                   ;; if this function takes only one argument, and all the effects are about that one argument
                   [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
                    => (lambda (thn/els) (make-arr arg-types t #f (car thn/els) (cdr thn/els)))]
                   ;; otherwise, the simple case
                   [else (make-arr arg-types t)])]
                [_ (int-err "bad match")]))))]
      [(args* ... . rest)
       (let ([t (tc/lambda-clause args body)])               
         (check-below (make-Function (list t)) (make-Function (list (make-arr arg-tys ret-ty rest-ty))))
         t)]))
      
  
  (define (tc/lambda-clause args body)
    (syntax-case args ()
      [(args ...)       
       (let* ([arg-list (syntax->list #'(args ...))]
              [arg-types (map get-type arg-list)])
         (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) arg-list)
         (with-lexical-env/extend 
          arg-list arg-types
          (match (tc-exprs (syntax->list body))
            [(tc-result: t thn els)
             (cond
               ;; this is T-AbsPred                
               ;; if this function takes only one argument, and all the effects are about that one argument
               [(and (= 1 (length arg-list)) (remove-var (car arg-list) thn els))
                => (lambda (thn/els) (make-arr arg-types t #f (car thn/els) (cdr thn/els)))]
               ;; otherwise, the simple case
               [else (make-arr arg-types t)])]
            [_ (int-err "bad match")])))]
      [(args ... . rest)
       (let* ([arg-list (syntax->list #'(args ...))]
              [arg-types (map get-type arg-list)]
              [rest-type (get-type #'rest)])
         (for-each (lambda (a) (printf/log "Lambda Var: ~a~n" (syntax-e a))) (cons #'rest arg-list))
         (with-lexical-env/extend 
          (cons #'rest arg-list) 
          (cons (make-Listof rest-type) arg-types)
          (match-let ([(tc-result: t thn els) (tc-exprs (syntax->list body))])
            (make-arr arg-types t rest-type))))]))

  ;(trace tc-args)
  
  ;; tc/mono-lambda : syntax-list syntax-list -> Funty
  ;; typecheck a sequence of case-lambda clauses
  (define (tc/mono-lambda formals bodies expected)
    (define (syntax-len s)
      (cond [(syntax->list s) => length]
            [else (let loop ([s s])
                    (if (pair? (syntax-e s))
                        (+ 1 (loop (cdr (syntax-e s))))
                        1))]))
    (if (and expected
             (= 1 (length (syntax->list formals))))
        ;; special case for not-case-lambda
        (match expected
          [(Function: (list (arr: args ret rest _ _))) 
           (tc/lambda-clause/check (car (syntax->list formals)) (car (syntax->list bodies)) args ret rest)]
          [_ (error "foo!")])
        (let loop ([formals (syntax->list formals)] 
                   [bodies (syntax->list bodies)]
                   [formals* null]
                   [bodies* null]
                   [nums-seen null])
          (cond 
            [(null? formals)
             (make-Function (map tc/lambda-clause (reverse formals*) (reverse bodies*)))]
            [(memv (syntax-len (car formals)) nums-seen)
             ;; we check this clause, but it doesn't contribute to the overall type
             (tc/lambda-clause (car formals) (car bodies))
             (loop (cdr formals) (cdr bodies) formals* bodies* nums-seen)]
            [else
             (loop (cdr formals) (cdr bodies) 
                   (cons (car formals) formals*)
                   (cons (car bodies) bodies*)
                   (cons (syntax-len (car formals)) nums-seen))]))))
  
  (define (tc/lambda form formals bodies)
    (tc/lambda/internal form formals bodies #f))
  
  ;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
  ;; tc/lambda/internal syntax syntax-list syntax-list option[type] -> Type
  (define (tc/lambda/internal form formals bodies expected)
    (if (syntax-property form 'typechecker:plambda)
        (tc/plambda form formals bodies expected)
        (tc/mono-lambda formals bodies expected)))
  
  (define (tc/lambda/check form formals bodies expected)
    (tc/lambda/internal form formals bodies expected))
  
  ;; tc/plambda syntax syntax-list syntax-list -> Poly
  ;; formals and bodies must by syntax-lists
  (define (tc/plambda form formals bodies expected)
    (with-syntax ([tvars (syntax-property form 'typechecker:plambda)])
      (let* ([literal-tvars (map syntax-e (syntax->list #'tvars))]
             [new-tvars (map make-F literal-tvars)]
             [ty (parameterize ([current-tvars (extend-env literal-tvars new-tvars (current-tvars))])
                   (tc/mono-lambda formals bodies))])
        ;(printf "plambda: ~a ~a ~a ~n" literal-tvars new-tvars ty)
        (ret (make-Poly literal-tvars ty)))))
  
  (define (tc/rec-lambda/check form formals body name args ret)
    (with-lexical-env/extend
     (syntax->list formals) args
     (let ([t (->* args ret)])
       #;(printf "fooing!~n")
       (with-lexical-env/extend
        (list name) (list t)
        (begin (tc-exprs/check (syntax->list body) ret)
               #;(printf "garg: ~a~n" t)
               (make-Function (list t)))))))
  
  ;(trace tc/mono-lambda)
  
  
  )