#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module poly-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  #;(require "prims.ss")
  (define: mymap : (All (a b) ((a -> b) (list-of a) -> (list-of b)))
    (plambda: (a b) ([f : (a -> b)] [l : (list-of a)])
      (cond [(null? l) '()]
            [else (cons (f (car l))
                         (mymap f #{(cdr l) :: (Listof a)}))])))
  
  (pdefine: (a b) (mymap2 [f : (a -> b)] [l : (list-of a)]) : (list-of b)
    (cond [(null? l) '()]
          [else (cons (f (car l))
                      (mymap2 f #{(cdr l) :: (Listof a)}))]))
  
  (define: x : (list-of number) 
    (mymap (lambda: ([x : number]) (+ 3 x)) (cons 1 (cons 4 #{'() : (list-of number)}))))

  (define: x2 : (list-of number) 
    (mymap2 (lambda: ([x : number]) (+ 3 x)) (cons 1 (cons 4 #{'() : (list-of number)}))))
  
  (provide x2)
  
  
  )