(module poly-struct (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct (a) bag ([val : a]))
  
  (provide make-bag)
    
  (let: ([x : (bag number) (make-bag 3)]
         [y : (bag boolean) (make-bag #t)])
    (+ 4 (bag-val x))
    (not (bag-val y))))
