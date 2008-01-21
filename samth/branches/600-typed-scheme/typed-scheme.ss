#lang scheme/base

(require "private/prims.ss" 
         "private/init-envs.ss"
         "private/extra-procs.ss"
         "private/internal-forms.ss"
         "private/base-env.ss"
         (for-syntax 
          scheme/base
          "private/type-utils.ss"
          "private/typechecker.ss" 
          "private/type-rep.ss"
          "private/provide-handling.ss"
          "private/type-environments.ss" "private/tc-utils.ss"
          "private/type-env.ss" "private/type-name-env.ss"
          "private/utils.ss"
          "private/internal-forms.ss"
          "private/init-envs.ss"
          "private/type-effect-convenience.ss"
          "private/effect-rep.ss"
          "private/rep-utils.ss"
          syntax/kerncase
          mzlib/list
          mzlib/plt-match))


(provide 
 ;; provides syntax such as define: and define-typed-struct
 (all-from-out "private/prims.ss") 
 ;; provides some pointless procedures - should be removed
 (all-from-out "private/extra-procs.ss"))

(provide-tnames)
(provide-extra-tnames)

(provide (except-out (all-from-out scheme/base) 
                     eval
                     with-handlers
                     #%module-begin
                     #%top-interaction
                     #%app
                     lambda
                     require))
(provide (rename-out [module-begin #%module-begin]
                     [with-handlers: with-handlers]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%plain-app #%app]
                     [require require]))

(define-for-syntax catch-errors? #f)


(define-syntax (module-begin stx)
  (define module-name (syntax-property stx 'enclosing-module-name))
  (with-logging-to-file 
   (log-file-name (syntax-src stx) module-name)
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
                                                           (cons (syntax-e id) ty))))]
                          ;; reinitialize seen type variables
                          [type-name-references null])
             (do-time "Initialized Envs")
             (with-syntax* (;; local-expand the module
                            ;; pmb = #%plain-module-begin
                            [(pmb body2 ...) 
                             (local-expand #`(#%plain-module-begin 
                                              forms ...)
                                           'module-begin 
                                           null
                                           #;stop-list)]
                            [__ (do-time "Local Expand Done")]
                            ;; typecheck the body, and produce syntax-time code that registers types
                            [extra-code (type-check #'(body2 ...))]
                            [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))]
                            [(transformed-body2 ...) (remove-provides #'(body2 ...))])
               (do-time "Typechecked")
               (printf "checked ~a~n" module-name)
               #;(printf "created ~a types~n" (count!))
               #;(printf "tried to create ~a types~n" (all-count!))
               #;(printf "created ~a union types~n" (union-count!))
               ;; reconstruct the module with the extra code
               #'(pmb transformed-body2 ... extra-code check-syntax-help))))))))))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . (module . rest))
     (eq? 'module (syntax-e #'module))
     #'(module . rest)]
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
         ;(do-time "Initialized Envs")
         (let* (;; local-expand the module
                [body2 (local-expand #'(#%top-interaction . form) 'top-level null)]
                ;[__ (do-time "Local Expand Done")]
                ;; typecheck the body, and produce syntax-time code that registers types
                [type (tc-toplevel-form body2)])
           ;(do-time "Typechecked")
           ;(printf "checked ~a~n" (syntax-property stx 'enclosing-module-name))
           (kernel-syntax-case body2 #f
             [(head . _)
              (or (free-identifier=? #'head #'define-values)
                  (free-identifier=? #'head #'define-syntaxes)
                  (free-identifier=? #'head #'require)
                  (free-identifier=? #'head #'provide)
                  (free-identifier=? #'head #'begin))
              body2]
             ;; reconstruct the module with the extra code
             [_ (with-syntax ([b body2]
                              [ty-str (match type
                                        [(tc-result: t thn els)
                                         (format "- : ~a\n" t)]
                                        [x (error 'internal-typechecker "bad type result: ~a" x)])])                    
                  (if (equal? -Void (tc-result-t type))
                      #'b
                      #`(let ([v b] [type 'ty-str])
                          (begin0 
                            v
                            (printf type)))))])))))))



