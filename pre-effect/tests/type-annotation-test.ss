(module type-annotation-test mzscheme
  (require "test-utils.ss")
  (require/private planet-requires type-annotation tc-utils types types-aux type-environments
                   parse-type)
  
  (require-libs)
  (require-schemeunit)
  
  (provide type-annotation-tests)
  
  (define-syntax (tat stx)
    (syntax-case stx ()
      [(_ ann-stx ty)
       #`(check-type-equal? #,(format "~a" (syntax-object->datum #'ann-stx))  
                            (parameterize ([current-type-names initial-type-names])
                              (type-annotation #'ann-stx))
                            ty)]))

  #reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
  (define (type-annotation-tests)
    (test-suite 
     "Type Annotation tests"
     
     (tat #{foo : number} N)
     (tat foo #f)
     (tat #{foo : 3} (make-value 3))))
  
  (define-go
    type-annotation-tests)

  )