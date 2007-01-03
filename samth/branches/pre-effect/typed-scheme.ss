(module typed-scheme mzscheme
  
  (require "private/prims.ss" "private/extra-procs.ss")
  
  (require-for-syntax "private/typechecker.ss" 
                      "private/type-environments.ss" "private/tc-utils.ss"
                      "private/type-env.ss" "private/type-name-env.ss"
                      "private/base-env.ss"
                      "private/utils.ss")

  (provide (all-from "private/prims.ss") 
           (all-from "private/extra-procs.ss"))
    
  (provide (all-from-except mzscheme #%module-begin))
  (provide (rename module-begin #%module-begin))
  
  (define-for-syntax stop-list (list #'define-type-alias-internal 
                                     #'define-typed-struct-internal
                                     #'define-type-internal
                                     #'require/typed-internal))
  
 (define-syntax (module-begin stx)
    (syntax-case stx ()                     
      ((_ forms ...)
       (begin
         ;; add initial value bindings - this table is updated
         (initialize-type-env)
         ;; add initial type bindings - this table is updated
         (initialize-type-name-env)
         (parameterize ([current-tvars initial-tvar-env]
                        [current-type-names
                         (lambda () (type-name-env-map (lambda (id ty)
                                                         (cons (syntax-e id) ty))))])        
           (with-syntax* ([(pmb rfs body2 ...) (local-expand #'(#%module-begin forms ...) 'module-begin stop-list)]
                          [extra-code (type-check #'(body2 ...))])
             (printf "checked ~a~n" (syntax-property stx 'enclosing-module-name))
             #'(pmb rfs body2 ... extra-code)))))))
  
  )


