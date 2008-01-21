#lang scheme/base

(provide (rename-out [body #%module-begin])
         (rename-out [body #%plain-module-begin]))

(require (for-syntax scheme/base))

(define-syntax (body stx)
  (syntax-case stx ()
    [(b main spec ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (except-in main #%module-begin)                 
                 (only-in main [#%module-begin main:#%module-begin]))
        
        (provide (all-from-out main)
                 (rename-out [main:#%module-begin #%module-begin]))
        (require spec ...)
        (provide (all-from-out spec) ...)))]))

 
