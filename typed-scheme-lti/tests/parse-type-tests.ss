(module parse-type-tests mzscheme
  (require "test-utils.ss")
  (require/private planet-requires type-comparison parse-type type-rep
                   type-effect-convenience tc-utils type-environments
                   type-name-env init-envs union base-env)
  
  
  (require-libs)
  (require-schemeunit)
  
  (provide parse-type-tests)
  
  (define-syntax (run-one stx)
    (syntax-case stx ()
      [(_ ty) #'(parameterize ([current-tvars initial-tvar-env])
                  (initialize-type-name-env initial-type-names)
                  (parse-type (quote-syntax ty)))]))
  
  (define-syntax (pt-test stx)
    (syntax-case stx ()
      [(_ ts tv) #'(pt-test ts tv () initial-tvar-env)]
      [(_ ts tv tys) #'(pt-test ts tv tys initial-tvar-env)]
      [(_ ty-stx ty-val ((nm ty) ...) tvar-env)
       #`(test-case #,(format "~a" (syntax-object->datum #'ty-stx))
                    (parameterize ([current-tvars tvar-env])
                      #;(initialize-type-name-env initial-type-names)
                      (register-type-name #'nm ty) ...
                      (check type-equal? (parse-type (quote-syntax ty-stx)) ty-val)))]))
  
  (define-syntax pt-tests
    (syntax-rules ()
      [(_ nm [elems ...] ...)
       (test-suite nm
                   (pt-test elems ...) ...)]))

  (define (parse-type-tests)  
    (pt-tests
     "parse-type tests" 
     [number N]
     [Any Univ]
     [(All (Number) Number) (-poly (a) a)]
     [(number . number) (-pair N N)]
     [(list-of boolean) (make-Listof  B)]
     [(vector-of (list-of symbol)) (make-Vector (make-Listof Sym))]
     [(pred number) (make-pred-ty N)]
     [(values number boolean number) (-values (list N B N))]
     [(number -> number) (-> N N)]
     [(Number -> Number) (-> N N)]
     [(number number number boolean -> number) (N N N B . -> . N)]
     [(number number number .. -> boolean) ((list N N) N . ->* . B)]
     ;[((. number) -> number) (->* (list) N N)] ;; not legal syntax
     [(Un number boolean) (Un N B)]
     [(Un number boolean number) (Un N B)]
     [(Un number boolean 1) (Un N B)]
     [(All (a) (list-of a)) (-poly (a) (make-Listof  a))]
     [(case-lambda (number -> boolean) (number number -> number)) (cl-> [(N) B]
                                                                        [(N N) N])]
     [1 (-val 1)]
     [#t (-val #t)]
     [#f (-val #f)]
     ["foo" (-val "foo")]
     
     [(poly-lst number) (make-Listof  N) ((poly-lst (-poly (a) (make-Listof  a))))
                        #;(extend-env (list 'poly-lst) (list (-poly (a) (make-Listof  a))) initial-type-names)]
     
     [a (-v a) () (extend-env (list 'a) (list (-v a))
                                      initial-tvar-env)]
     
     ))
     
  
  (define (go)
    (run parse-type-tests))
  

  
  )