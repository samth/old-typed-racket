(module all-tests mzscheme
  
  (require 
   "test-utils.ss"
   "subtype-tests.ss" ;; done
   "type-equal-tests.ss" ;; done
   "remove-intersect-tests.ss" ;; done
   "parse-type-tests.ss" ;; done
   "type-annotation-test.ss" ;; done
   "typecheck-tests.ss"
   "module-tests.ss"
   "infer-tests.ss")
  
  (require-for-syntax (lib "etc.ss"))
  
  (require-for-syntax/private utils)
  
  (require/private planet-requires)
  
  (require-schemeunit)
  
  (provide (all-defined))
 
  (define-syntaxes (it it/r it/fail)
      (let ()
        (define ((mk fun nm fail?) stx)
          (syntax-case stx ()
            [(_ files ...)
             (with-syntax* ([(files* ...) (map fun (syntax->list #'(files ...)))]
                            [(thunks ...) #`((lambda () (parameterize ([read-accept-reader #t]
                                                                       [current-load-relative-directory 
                                                                        #,(this-expression-source-directory)])
                                                          files*)) ...)]
                            [(tests ...) (if fail? 
                                             #'((test-exn files exn:fail:syntax? thunks) ...)
                                             #'((test-not-exn files thunks) ...))])
               #`(test-suite #,(string-append "Integration Tests - " nm)
                             tests ...))]))
        (values (mk (lambda (f) #`(load #,f)) "Load" #f)
                (mk (lambda (f) #`(dynamic-require '(file #,f) #f)) "Require" #f)
                (mk (lambda (f) #`(dynamic-require '(file #,f) #f)) "Fail" #t))))
  
  (define (integration-tests)
    (test-suite 
     "Integration Tests"                
     (it
      "foo.scm"
      "manual-examples.ss")
     (it/r
      "scratch.ss"
      "scratch2.ss"
      "vec-tests.ss" 
      "cl-tests.ss"
      "rec-types.ss"
      "struct-exec.ss"
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
      "set-struct.ss"
      "do.ss"
      "struct-exec.ss"
      ;; galore tests
      "batched-queue.scm"
      "leftist-heap.ss"
      "priority-queue.scm"
      ;; eli tests
      "660-examples/hw01.scm"
      "660-examples/hw02.scm"
      ;; these are too slow
      #;"660-examples/hw03.scm"
      #;"660-examples/hw04.scm")
     (it/fail "set-tests.ss"
              "internal-fail.ss"))
     )
  
  (define-go
    subtype-tests
    type-equal-tests
    restrict-tests
    remove-tests
    parse-type-tests
    type-annotation-tests
    typecheck-tests
    module-tests
    fv-tests
    i2-tests
    combine-tests
    integration-tests) 
  
  (define (fast)
    (run     
     subtype-tests
     type-equal-tests
     restrict-tests
     remove-tests
     parse-type-tests
     type-annotation-tests
     typecheck-tests
     module-tests
     fv-tests
     i2-tests
     combine-tests))
  
  (define (faster)
    (run     
     subtype-tests
     type-equal-tests
     restrict-tests
     remove-tests
     parse-type-tests
     type-annotation-tests
     fv-tests
     i2-tests
     combine-tests))
  
  ;(go)
  
  
  )