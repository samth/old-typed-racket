(module typed-scheme-660 mzscheme
  
  (require "private/prims.ss" 
           "private/extra-procs.ss"
           "private/internal-forms.ss"
           (prefix 6: "optimize/csu660.ss")
           )

  (require-for-template "private/prims.ss" "private/extra-procs.ss" "private/internal-forms.ss" mzscheme)
  
  (require-for-syntax "private/typechecker.ss" 
                      "private/type-rep.ss"
                      "private/type-environments.ss"
                      "private/tc-utils.ss"
                      "private/type-effect-convenience.ss"
                      "private/type-env.ss" 
                      "private/type-name-env.ss"
                      "private/660-env.ss"
                      "private/utils.ss"
                      "private/internal-forms.ss"
                      "private/init-envs.ss"
                      "private/effect-rep.ss"
                      "private/type-comparison.ss"
                      (lib "kerncase.ss" "syntax")
                      (lib "list.ss")
                      (lib "plt-match.ss"))

  
  (provide 
   ;; provides syntax such as define: and define-typed-struct
   (all-from "private/prims.ss"))
    
  #;(provide (all-from-except "optimize/csu660.ss" #;with-handlers #%module-begin #%top-interaction))
  (provide (rename module-begin #%module-begin)
           (rename with-handlers: with-handlers)
           (rename top-interaction #%top-interaction)
           #%app #%datum #%top require)
  
  (begin-for-syntax
    ;; add initial value bindings - this table is updated
    (initialize-type-env initial-env)
    ;; add initial type bindings - this table is updated
    (initialize-type-name-env initial-type-names)
    ;; initialize other bindings
    (initialize-others))
  
  (define-for-syntax catch-errors? #f)
  
  (define-for-syntax (remove-provides forms)
    (filter
     (lambda (f) (kernel-syntax-case f #f
                   [(provide . _) #f]
                   [_ #t]))
     (syntax->list forms)))


  (define-syntax (module-begin stx)
    (define module-name (syntax-property stx 'enclosing-module-name))
    (with-logging-to-file (log-file-name (syntax-source stx) module-name)
      (syntax-case stx ()                     
        ((mb forms ...)
         (begin
           (set-box! typed-context? #t)
           (start-timing module-name)
           (call-with-exception-handler
            (lambda (e)
              (if (and catch-errors? (exn:fail? e) (not (exn:fail:syntax? e)))
                  (with-handlers ([values values])
                    (tc-error "Internal error: ~a" e))
                  e))
            (lambda ()
              (parameterize (;; this paramter is for parsing types
                             [current-tvars initial-tvar-env]
                             ;; this parameter is just for printing types
                             ;; this is a parameter to avoid dependency issues
                             [current-type-names
                              (lambda () (type-name-env-map (lambda (id ty)
                                                              (cons (syntax-e id) ty))))])           
                (do-time "Initialized Envs")
                (with-syntax* (;; local-expand the module
                               ;; pmb = #%plain-module-begin
                               ;; rfs = (require-for-syntax mzscheme)
                               [(pmb rfs body2 ...) 
                                (local-expand #`(#%plain-module-begin 
                                                 #,(syntax-local-introduce #'(require-for-syntax mzscheme)) 
                                                 forms ...)
                                              'module-begin 
                                              null
                                              #;stop-list)]
                               [__ (do-time "Local Expand Done")]
                               ;; typecheck the body, and produce syntax-time code that registers types
                               [extra-code (type-check #'(body2 ...))]
                               [(transformed-body2 ...) (remove-provides #'(body2 ...))])
                              (do-time "Typechecked")
                              (printf "checked ~a~n" module-name)
                              ;; reconstruct the module with the extra code
                              #'(pmb rfs transformed-body2 ... extra-code))))))))))
  
  (define-syntax (top-interaction stx)
    (syntax-case stx (module)
      [(_ module . rest) #'(module . rest)]
      ((_ . form)
       (begin
         (set-box! typed-context? #t)
         (parameterize (;; this paramter is for parsing types
                        [current-tvars initial-tvar-env]
                        ;; this parameter is just for printing types
                        ;; this is a parameter to avoid dependency issues
                        [current-type-names
                         (lambda () (type-name-env-map (lambda (id ty)
                                                         (cons (syntax-e id) ty))))])       
           (do-time "Initialized Envs")
           (let* (;; local-expand the module
                  [body2 (local-expand #'(#%top-interaction . form) 'top-level null)]
                  [__ (do-time "Local Expand Done")]
                  ;; typecheck the body, and produce syntax-time code that registers types
                  [type (tc-toplevel-form body2)])
             (do-time "Typechecked")
             (begin0
               (kernel-syntax-case body2 ()
                 [(head . _)
                  (or (module-identifier=? #'head #'define-values)
                      (module-identifier=? #'head #'define-syntaxes)
                      (module-identifier=? #'head #'require)
                      (module-identifier=? #'head #'provide)
                      (module-identifier=? #'head #'require-for-template)
                      (module-identifier=? #'head #'require-for-syntax)
                      (module-identifier=? #'head #'begin))
                  body2]
                 ;; reconstruct the module with the extra code
                 [_ (with-syntax ([b body2]
                                  [ty-str (match type
                                            [(tc-result: t thn els)
                                             (format "- : ~a\n" t)]
                                            [x (printf "~a~n" x) ""])])                    
                      (if (type-equal? -Void (tc-result-t type))
                          #'b
                          #`(let ([v b] [type 'ty-str])
                              (begin0 
                                v
                                (printf ty-str)))))])
               (do-time "Top-level Done"))))))))
  
  )


