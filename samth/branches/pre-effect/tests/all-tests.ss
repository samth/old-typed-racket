(module all-tests mzscheme
  
  (require 
   "test-utils.ss"
   "subtype-tests.ss"
   "type-equal-tests.ss"
   "remove-intersect-tests.ss"
   "parse-type-tests.ss"
   "type-annotation-test.ss"
   "typecheck-tests.ss"
   "module-tests.ss"
   "infer-tests.ss")
  
  (require/private planet-requires)
  
  (require-schemeunit)
  
  (provide (all-defined))
  
  (define (integration-tests)
    (define-syntaxes (it it/r)
      (let ()
        (define ((mk fun nm) stx)
          (syntax-case stx ()
            [(_ files ...)
             (with-syntax ([(files* ...) (map fun (syntax->list #'(files ...)))])
               #`(test-suite #,(string-append "Integration Tests - " nm)
                             (test-not-exn files
                                           (lambda () 
                                             (parameterize ([read-accept-reader #t])
                                               files*))) ...))]))
        (values (mk (lambda (f) #`(load #,f)) "Load")
                (mk (lambda (f) #`(dynamic-require #,f #f)) "Require"))))
    (it
     "foo.scm"
     "manual-examples.ss")
    (it/r
     "scratch.ss"
     "vec-tests.ss" 
     "cl-tests.ss"
     "rec-types.ss"
     "annotation-test.ss"    
     "poly-struct.ss"
     "basic-tests.ss"
     "little-schemer.ss"
     "seasoned-schemer.ss"
     "poly-tests.ss"
     "let-values-tests.ss"
     "if-splitting-test.ss"
     "pair-test.ss"
     "barland.ss"
     "mu-rec.ss"
     ;; galore tests
     "batched-queue.scm"
     "leftist-heap.ss")
    )
  
  (define-go
    subtype-tests
    type-equal-tests
    intersect-tests
    remove-tests
    parse-type-tests
    type-annotation-tests
    typecheck-tests
    module-tests
    fv-tests
    integration-tests)
  
  
  
  )