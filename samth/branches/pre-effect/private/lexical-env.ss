(module lexical-env mzscheme
  
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))
  (require "type-environments.ss" "tc-utils.ss" "type-env.ss")
  
  (provide (all-defined))
  
  (define lexical-env (make-parameter (make-empty-env module-identifier=?)))
  
  (define-syntax with-lexical-env
    (syntax-rules ()
      [(_ e . b) (parameterize ([lexical-env e]) . b)]))
  
  (define-syntax with-lexical-env/extend
    (syntax-rules ()
      [(_ is ts . b) (parameterize ([lexical-env (extend/values is ts (lexical-env))]) . b)]))

  
  (define (lookup-type/lexical i)
    (lookup (lexical-env) i 
            (lambda (i) (lookup-type i))))
  
  (define (update-type/lexical f i)
    (define (update f k env)
      (parameterize
          ([current-orig-stx k])
        (let* ([v (lookup-type/lexical k)]
               [new-v (f k v)]
               [new-env (extend env k new-v)])
          new-env)))
    (update f i (lexical-env)))
  
  (define-syntax with-update-type/lexical
    (syntax-rules ()
      [(_ f i . b)
       (with-lexical-env (update-type/lexical f i) . b)]))

  )