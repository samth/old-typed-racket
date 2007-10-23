(module type-annotation-test mzscheme
  (require "test-utils.ss")
  (require/private planet-requires type-annotation tc-utils type-rep type-effect-convenience type-environments
                   parse-type base-env init-envs type-name-env)
  
  (require-libs)
  (require-schemeunit)
  
  (provide type-annotation-tests)
  
  (define-syntax (tat stx)
    (syntax-case stx ()
      [(_ ann-stx ty)
       #`(check-type-equal? #,(format "~a" (syntax-object->datum #'ann-stx))  
                            (parameterize ([current-type-names initial-type-names])
                              (initialize-type-name-env initial-type-names)
                              (type-annotation #'ann-stx))
                            ty)]))

  #reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
  (define (type-annotation-tests)
    (test-suite 
     "Type Annotation tests"
     
     (tat #{foo : Number} N)
     (tat foo #f)
     (tat #{foo : 3} (-val 3))))
  
  (define-go
    type-annotation-tests)

  )