(module module-tests mzscheme
  (require "test-utils.ss")
  (require/private planet-requires)
  (require-schemeunit)
  
  (provide module-tests)
  
  (define (module-tests)
    (test-suite "Tests for whole modules"
                #;(test-not-exn "name" (lambda () (expand #'(module m (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
                                                            (define: x : number 3)))))
                ))
  
  
  (define-go module-tests)
  
  )