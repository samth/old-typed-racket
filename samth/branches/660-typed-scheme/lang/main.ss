#lang s-exp "minimal.ss"

(require (except-in "../typed-scheme.ss" #%module-begin lambda #%app with-handlers #%top-interaction)
         (prefix-in main: "../typed-scheme.ss")
         (only-in scheme sqr)
         (except-in CSU660/csu660 require #%module-begin lambda #%app #%top-interaction list-of))

(provide (all-from-out "../typed-scheme.ss")
         (all-from-out CSU660/csu660)
         require
         sqr
         (rename-out [main:#%module-begin #%module-begin]
                     [main:lambda lambda]
                     [main:#%app #%app]
                     [main:with-handlers with-handlers]
                     [main:#%top-interaction #%top-interaction]))
