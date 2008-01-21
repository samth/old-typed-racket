#lang s-exp "../minimal.ss"

            
(require (except-in "../../typed-scheme-660.ss" #%module-begin lambda #%app with-handlers #%top-interaction cons?)         
         (except-in "../../optimize/csu660.ss" list-of if list? cons)
         (prefix-in 6: "../../optimize/csu660.ss")
         (prefix-in main: "../../typed-scheme.ss"))

(provide (all-from-out "../../typed-scheme-660.ss")
         (all-from-out "../../optimize/csu660.ss")
         (rename-out 
          [6:if if]          
          [6:list? list?]
          [6:cons cons]
          [main:#%module-begin #%module-begin]
          [main:lambda lambda]
          [main:#%app #%app]
          [main:with-handlers with-handlers]
          [main:#%top-interaction #%top-interaction]))

