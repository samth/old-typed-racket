(module typecheck-tests mzscheme
  
  (require "test-utils.ss")
  (require/private planet-requires typechecker
                   types types-aux type-env base-env
                   prims type-environments tc-utils
                   type-name-env)  
  
  (require-for-syntax/private tc-utils typechecker)
  

  (require-libs)
  (require-schemeunit)
  
  (provide typecheck-tests tc-expr/expand)
  
  (require-for-template mzscheme)
  
  (define-syntax tc-l
    (syntax-rules ()
      [(_ lit ty)
       (check-type-equal? (format "~a" 'lit) (tc-literal #'lit) ty)]))
  
  (define-syntax (tc-expr/expand stx)
    (syntax-case stx ()
      [(_ e)
       (with-syntax ([e* (local-expand #'e 'expression ())])
         #'(begin
             (initialize-type-name-env)
             (tc-expr #'e*)))]))
  
  (define-syntax (tc-e stx)
    (syntax-case stx ()
      [(_ expr ty)
       #`(check-type-equal? (format "~a" 'expr) 
                            (eval-syntax #'(tc-expr/expand expr))                            
                            ty)]))
  
  (require-for-syntax (lib "kerncase.ss" "syntax"))
  
  (define-for-syntax (local-expand/top-level form)
    (let ([form* (local-expand form 'module (kernel-form-identifier-list #'here))])
      (kernel-syntax-case form* #f
        [(define-syntaxes . _) (raise-syntax-error "don't use syntax defs here!")]
        [(define-values vals body)
         (quasisyntax/loc form (define-values vals #,(local-expand #'body 'expression '())))]
        [e (local-expand #'e 'expression '())])))
  
  #;(define-syntax (tc-tl stx)
    (syntax-case stx ()
      [(_ form)
       (with-syntax ([form* (local-expand/top-level #'form)])
         #'(test-not-exn (format "~a" 'form) 
                         (lambda ()
                           (initialize-type-name-env)
                           (tc-toplevel #'form*))))]))
  
  #;(define-syntax (run-tl stx)
    (syntax-case stx ()
      [(_ form)
       (with-syntax ([form* (local-expand/top-level #'form)])
         #'(begin
             (initialize-type-env)
             (initialize-type-name-env)
             (tc-toplevel #'form*)))]))
   
  (define-syntax (tc-err stx)
    (syntax-case stx ()
      [(_ expr)
       (with-syntax ([expr* (local-expand #'expr 'top-level '())])
         #'(test-exn (format "~a" 'expr) 
                     exn:fail:syntax?                     
                     (lambda ()
                       (initialize-type-name-env)
                       (tc-expr #'expr*))))]))
                      
  
  (define (typecheck-tests)
    (test-suite 
     "Typechecker tests"
     #reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
     (test-suite
      "tc-expr tests"
      #:before
      (lambda () (for-each (lambda (nm/ty) (register-type (car nm/ty) (cadr nm/ty))) initial-env))
      (tc-e 3 N)
      (tc-e "foo" String)
      (tc-e (+ 3 4) N)
      [tc-e (lambda: () 3) (-> N)]
      [tc-e (lambda: ([x : number]) 3) (-> N N)]
      [tc-e (lambda: ([x : number] [y : boolean]) 3) (-> N B N)]
      [tc-e (lambda () 3) (-> N)]
      [tc-e (values 3 4) (make-values-ty (list N N))]
      [tc-e (cons 3 4) (make-pair-ty N N)]
      [tc-e (cons 3 #{'() : (list-of number)}) (make-Listof  N)]
      [tc-e (void) Void]
      [tc-e (void 3 4) Void]
      [tc-e (void #t #f '(1 2 3)) Void]
      [tc-e #(3 4 5) (make-vec N)]
      [tc-e '(2 3 4) (make-Listof  N)]
      [tc-e '(2 3 #t) (make-Listof  (Un N B))]
      [tc-e #(2 3 #t) (make-vec (Un N B))]
      [tc-e '(#t #f) (make-Listof  B)]
      [tc-e (plambda: (a) ([l : (list-of a)]) (car l))
            (make-poly '(a) (-> (make-Listof  (make-tvar 'a)) (make-tvar 'a)))]
      [tc-e #{(lambda: ([l : (list-of a)]) (car l)) PROP typechecker:plambda (a)}
            (make-poly '(a) (-> (make-Listof  (make-tvar 'a)) (make-tvar 'a)))]
      [tc-e (case-lambda: [([a : number] [b : number]) (+ a b)]) (-> N N N)]
      [tc-e (let: ([x : number 5]) x) N]
      [tc-e (let-values ([(x) 4]) (+ x 1)) N]
      [tc-e (let-values ([(#{x : number} #{y : boolean}) (values 3 #t)]) (and (= x 1) (not y))) B]
      [tc-e (values 3) N]
      [tc-e (values) (make-values-ty (list))]
      [tc-e (values 3 #f) (make-values-ty (list N (make-value #f)))]
      [tc-e (map #{values : (symbol -> symbol)} '(a b c)) (make-Listof  Sym)]
      [tc-e (letrec: ([fact : (number -> number) (lambda: ([n : number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                     (fact 20))
            N]
      [tc-e (let: fact : number ([n : number 20])
              (if (zero? n) 1 (* n (fact (- n 1)))))
            N]
      [tc-e (let: ([v : top 5])
                  (if (number? v) (+ v 1) 3))
            N]
      [tc-e (let: ([v : top #f])
                  (if (number? v) (+ v 1) 3))
            N]
      [tc-e (let: ([v : (Un number boolean) #f])
                  (if (boolean? v) 5 (+ v 1)))
            N]
      [tc-e (let: ([f : (number number -> number) +]) (f 3 4)) N]
      [tc-e (let: ([+ : (boolean -> number) (lambda: ([x : boolean]) 3)]) (+ #f)) N]
      [tc-e (if #f #t) (Un B Void)]
      [tc-e (if (number? #f) (+ 4 5)) (Un N Void)]
      [tc-e (let: ([x : (Un #f number) 7])
                  (if x (+ x 1) 3))
            N]
      [tc-e (begin 3) N]
      [tc-e (begin #f 3) N]
      [tc-e (begin #t) B]
      [tc-e (begin0 #t) B]
      [tc-e (begin0 #t 3) B]
      [tc-e (if #f 'a 3) (Un Sym N)]
      [tc-e (if #f #f #t) B]
      [tc-e (when #f 3) (Un N Void)]
      [tc-e '() (-val '())]
      [tc-e (lambda: ([x : number] . [y : number]) (car y)) (->* (list N) N N)]
      [tc-e ((lambda: ([x : number] . [y : number]) (car y)) 3) N]
      [tc-e ((lambda: ([x : number] . [y : number]) (car y)) 3 4 5) N]
      [tc-e ((lambda: ([x : number] . [y : number]) (car y)) 3 4) N]
      [tc-e (apply (lambda: ([x : number] . [y : number]) (car y)) 3 '(4)) N]
      [tc-e (apply (lambda: ([x : number] . [y : number]) (car y)) 3 '(4 6 7)) N]
      [tc-e (apply (lambda: ([x : number] . [y : number]) (car y)) 3 '()) N]
      
      [tc-e (lambda: ([x : number] . [y : boolean]) (car y)) (->* (list N) B B)]
      [tc-e ((lambda: ([x : number] . [y : boolean]) (car y)) 3) B]
      [tc-e (apply (lambda: ([x : number] . [y : boolean]) (car y)) 3 '(#f)) B]
      
      [tc-e (let: ([x : number 3])
                  (if (number? x) #t))
            B]      
      [tc-e (let: ([x : number 3])
                  (if (boolean? x) #t))
            Void]
      
      [tc-e (let: ([x : (Option number) #f]) x) (Un N (make-value #f))]
      [tc-e null (-val null)]

      ;; error tests
      [tc-err (+ 3 #f)]
      [tc-err (let: ([x : number #f]) x)]
      [tc-err (let: ([x : number #f]) (+ 1 x))]
      #;[tc-err (let: ([fact : (number -> number) (lambda: ([n : number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                     (fact 20))]
            
      #;[tc-err ]
      )
     (test-suite
      "check-type tests"
      (test-exn "Fails correctly" exn:fail:syntax? (lambda () (check-type #'here N B)))
      (test-not-exn "Doesn't fail on subtypes" (lambda () (check-type #'here N Univ)))
      (test-not-exn "Doesn't fail on equal types" (lambda () (check-type #'here N N)))
      )
     (test-suite
      "tc-literal tests"
      (tc-l 5 N)
      (tc-l 5# N)
      (tc-l 5.1 N)
      (tc-l #t B)
      (tc-l "foo" String)
      (tc-l foo Sym)
      (tc-l #:foo Keyword)
      (tc-l #f (make-value #f))
      (tc-l #"foo" Bytes)
      [tc-l () (-val null)]
      )
    ))
  
  
  ;; these no longer work with the new scheme for top-level identifiers
  #;(define (tc-toplevel-tests)
    #reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
    (test-suite "Tests for tc-toplevel"
                (tc-tl 3)
                (tc-tl (define: x : number 4))
                (tc-tl (define: (f [x : number]) : number x))
                [tc-tl (pdefine: (a) (f [x : a]) : number 3)]
                [tc-tl (pdefine: (a b) (mymap [f : (a -> b)] (l : (list-of a))) : (list-of b)
                                 (if (null? l) #{'() : (list-of b)}
                                     (cons (f (car l)) (map f (cdr l)))))]))

  
  (define-go typecheck-tests #;tc-toplevel-tests)
  
  )