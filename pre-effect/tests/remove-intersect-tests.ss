(module remove-intersect-tests mzscheme
  (require "test-utils.ss")
  (require/private types types-aux planet-requires remove-intersect)
  
  (require-libs)
  (require-schemeunit)

  (define-syntax (inter-tests stx)    
    (syntax-case stx ()
      [(_ [t1 t2 res] ...)
       #'(test-suite "Tests for intersect"
                      (test-check (format "Intersect test: ~a ~a" t1 t2) type-equal? (intersect t1 t2) res) ...)]))
  
  (define (intersect-tests) 
    (inter-tests
     [N (Un N Sym) N]
     [N N N]
     [(Un (-val 'foo) (-val 6)) (Un N Sym) (Un (-val 'foo) (-val 6))]
     [N (-mu a (Un N Sym (make-Listof a))) N]
     [(Un N B) (-mu a (Un N Sym (make-Listof a))) N]
     [(-mu x (Un N (make-Listof x))) (Un Sym N B) N]
     ))
  
  (define-syntax (remo-tests stx)    
    (syntax-case stx ()
      [(_ [t1 t2 res] ...)
       (syntax/loc stx
         (test-suite "Tests for remove"
                     (test-check (format "Remove test: ~a ~a" t1 t2) type-equal? (remove t1 t2) res) ...))]))
  
  (define (remove-tests)
    (remo-tests
     [(Un N Sym) N Sym]
     [N N (Un)]
     [(-mu x (Un N Sym (make-Listof x))) N (-mu x (Un Sym (make-Listof x)))]
     [(-mu x (Un N Sym B (make-Listof x))) N (-mu x (Un Sym B (make-Listof x)))]
     [(Un (-val #f) (-mu x (Un N Sym (make-Listof (make-tvar 'x)))))
      (Un B N) 
      (-mu x (Un Sym (make-Listof (make-tvar 'x))))]
     [(Un (-val 'foo) (-val 6)) (Un N Sym) (Un)]
     [(-> (Un Sym N) N) (-> N N) (Un)]
     [(Un (-poly (a) (make-Listof a)) (-> N N)) (-> N N) (-poly (a) (make-Listof a))]
     [(Un Sym N) (-poly (a) N) Sym]
     [(-pair N (-v a)) (-pair Univ Univ) (Un)]
     ))
  
  (define (go) 
    (run 
     intersect-tests
     remove-tests))
  
  (provide (all-defined-except go))

  )