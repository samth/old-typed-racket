(module type-equal-tests mzscheme
  (require "test-utils.ss")
  (require/private planet-requires types-aux types)
  (require-schemeunit)
  
  (provide type-equal-tests)
  
    
  (define-syntax (te-tests stx)
    (define (single-test stx)
      (syntax-case stx (FAIL)
        [(FAIL t s) #'((test-check (format "FAIL ~a" '(t s)) (lambda (a b) (not (type-equal? a b))) t s)
                       (test-check (format "FAIL ~a" '(s t)) (lambda (a b) (not (type-equal? a b))) s t))]
        [(t s) #'((test-check (format "~a" '(t s)) type-equal? t s)
                  (test-check (format "~a" '(s t)) type-equal? s t))]))
    (syntax-case stx ()
      [(_ cl ...)
       (with-syntax ([((cl1 cl2) ...) (map single-test (syntax->list #'(cl ...)))])
         #'(test-suite "Tests for type equality"
                       cl1 ... cl2 ...))]))
   
  (define (type-equal-tests)
    (te-tests
     [N N]
     [(Un N) N]
     [(Un N Sym) (Un Sym N)]
     [(-poly (x) (-> (Un Sym N) x)) (-poly (xyz) (-> (Un Sym N) xyz))]
     [(make-mu 'x (Un N Sym (make-tvar 'x))) (make-mu 'y (Un  Sym (Un N (Un (make-tvar 'y)))))]
     ;; found bug
     [FAIL (Un (-mu heap-node (-struct 'heap-node #f (list (-base 'comparator) N (-v a) (Un heap-node (-base 'heap-empty))))) 
               (-base 'heap-empty))
           (Un (-mu heap-node (-struct 'heap-node #f (list (-base 'comparator) N (-pair N N) (Un heap-node (-base 'heap-empty))))) 
               (-base 'heap-empty))]))
  
  

  (define-go
    type-equal-tests)

  
  )