(module lexical-env mzscheme
  
  (require "type-environments.ss" "tc-utils.ss" "type-env.ss" "mutated-vars.ss")
  
  (provide (all-defined))
  
  ;; the current lexical environment
  (define lexical-env (make-parameter (make-empty-env module-identifier=?)))
  
  ;; run code in a new env
  (define-syntax with-lexical-env
    (syntax-rules ()
      [(_ e . b) (parameterize ([lexical-env e]) . b)]))
  
  ;; run code in an extended env
  (define-syntax with-lexical-env/extend
    (syntax-rules ()
      [(_ is ts . b) (parameterize ([lexical-env (extend/values is ts (lexical-env))]) . b)]))

  ;; find the type of identifier i, looking first in the lexical env, then in the top-level env
  ;; identifer -> Type
  (define (lookup-type/lexical i)
    (lookup (lexical-env) i 
            (lambda (i) (lookup-type i))))
  
  ;; refine the type of i in the lexical env
  ;; (identifier type -> type) identifier -> environment
  (define (update-type/lexical f i)
    ;; do the updating on the given env
    ;; (identifier type -> type) identifier environment -> environment
    (define (update f k env)
      (parameterize
          ([current-orig-stx k])
        (let* ([v (lookup-type/lexical k)]
               [new-v (f k v)]
               [new-env (extend env k new-v)])
          new-env)))
    ;; check if i is ever the target of a set!
    (if (is-var-mutated? i)
        ;; if it is, we do nothing
        (lexical-env)
        ;; otherwise, refine the type
        (update f i (lexical-env))))
  
  ;; convenience macro for typechecking in the context of an updated env
  (define-syntax with-update-type/lexical
    (syntax-rules ()
      [(_ f i . b)
       (with-lexical-env (update-type/lexical f i) . b)]))

  )