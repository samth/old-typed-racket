
#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module basic-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  #|
  ;;syntax-only requires
  (require (only (lib "etc.ss") let+))
  (require (only (lib "match.ss") match))
  
  ;(provide (all-defined))
  (define: x* : Number 3)
  
  (define #{x** : number} 3)
  
  #{(and 3 4) :: (Un boolean number)}
  #{(and 3 4) :: (Un boolean number)}
   
  #;(define: match-test : number
    (match 3
      [(? number? #{x number}) (+ 17 x)]
      [_ 12]))
  
  
  (define-typed-struct pt ([x : Number] [y : number]))
  
  (define-typed-struct (3dpt pt) ([z : number]))
  
  (define: x-struct : pt (make-pt 3 4))
  
  (define: z-struct : 3dpt (make-3dpt 3 4 5))
  
  (define: z*-struct : pt z-struct)
  
  (define: x-mem : number (pt-x x-struct))
  
  (define: (pt-add [v : top]) : number
    (cond
      [(pt? v) (+ (pt-x v) (pt-y v))]
      [else 0]))

  (define: (zpt-add [v : top]) : number
    (cond
      [(3dpt? v) (+ (pt-x v) (pt-y v))]
      [else 0]))
  
  #;(define: (pt-add/match [v : top]) : number
    (match v
      [($ pt #{x number} #{y number}) (+ x y)]
      [_ 0]))
  
  #;(pt-add/match x-struct)
  
  #;(define-typed-struct pt ([z number]))
  
  ;; this had better not work
  #;(define: pt-unsound : boolean
    (cond [(pt? x-struct) (= (pt-z x-struct))]
          [else #t]))
  
  (define: a-bool : boolean (pt? 6))
  
  (define: blarz : number
    (let*: ([x : number 3]
            [y : number (+ x 1)])
           (add1 y)))
  
  (define: looping : number
    (let: loop : number ([a : number 1] [b : number 10]) (if (> a b) 1000 (loop (add1 a) (sub1 b)))))
  
  #;(make-pt 'x 'y)
  
  (define: x : number 3)
  (add1 x)
  #;(define-syntax foo
    (syntax-rules ()
      [(foo x1 y1) (= x1 y1)]))
  
  (define: (f [x : number] [y : number]) : number (+ x y))
  (define: (g [x : number] [y : number]) : boolean
    (let+ (#;[val #{z number} #f]
             [val #{x1 number} (* x x)]
             [rec #{y1 number} (* y y)])
          #|(define-syntax foo
            (syntax-rules ()
              [(foo) (= x1 y1)]))
          (foo)|#
          (= x1 y1)))
  (g (if (g (add1 x) (add1 (add1 x))) 10 100) 30)
  |#
  (define: mymap : (All (a b) ((a -> b) (list-of a) -> (list-of b)))
    (plambda: (a b) ([f : (a -> b)] [l : (list-of a)])
      (cond [(null? l) '()]
            [else (cons (f (car l))
                         (mymap f (cdr l)))])))
  
  (mymap add1 (cons 1 (cons 2 (cons 3 #{'() :: (Listof number)}))))
  
  )