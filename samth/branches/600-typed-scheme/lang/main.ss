#lang scheme/base

(require (except-in "../typed-scheme.ss" #%module-begin lambda #%app with-handlers #%top-interaction)
         (prefix-in main: "../typed-scheme.ss"))

(provide (all-from-out "../typed-scheme.ss")
         (rename-out [main:#%module-begin #%module-begin]
                     [main:lambda lambda]
                     [main:#%app #%app]
                     [main:with-handlers with-handlers]
                     [main:#%top-interaction #%top-interaction]))
