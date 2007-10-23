#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module poly-struct (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct (a) bag ([val : a]))
  
  (provide make-bag)
    
  (let: ([x : (bag number) (make-bag 3)]
         [y : (bag boolean) (make-bag #{#t :: boolean})])
    (+ 4 (bag-val x))
    (not (bag-val y))))
