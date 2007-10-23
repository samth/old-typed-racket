(module build-language mzscheme

 (provide (rename body #%module-begin)
          (rename body #%plain-module-begin))

 (define-syntax (body stx)
   (syntax-case stx ()
     [(b main spec ...)
      (syntax/loc stx
        (#%module-begin
         (require (all-except main #%module-begin #%plain-module-begin)
                  (rename main main:#%module-begin #%module-begin)
                  #;(rename main main:#%plain-module-begin #%plain-module-begin))
         (provide (all-from main)
                  (rename main:#%module-begin #%module-begin)
                  #;(rename main:#%plain-module-begin #%plain-module-begin))
         #;(provide (all-from main))
         (require spec ...)
         (provide (all-from spec) ...)))]))

 )
