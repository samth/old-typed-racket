#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module let-values-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (let-values ([(#{x : number} #{y : number}) (values 3 4)]
               [(#{z : number}) (values 3)]
               #;[(#{fact : (number -> number)})
                  (lambda: ([x : number])
                           (if (zero? x) 1 (* x (fact (- x 1)))))]
                 #;[(#{z : number}) (- x y)])
    (+ x y))

  (letrec-values ([(#{x : number} #{y : number}) (values 3 4)])
    (+ x y))
  (letrec-values ([(#{x : number} #{y : number}) (values 3 4)]
                  [(#{z : number}) (- x y)]
                  [(#{fact : (number -> number)})
                   (lambda: ([x : number])
                            (if (zero? x) 1 (* x (fact (- x 1)))))])
    (+ x y))
  
  (define-values (#{x : number} #{y : number}) (values 1 2))
  #;(define-values (#{z : number}) (values 1 2))
  
  )