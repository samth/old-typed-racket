(module m mzscheme
  (define x 3)
  (define (y z) (add1 z))
  (provide (all-defined)))


#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module bang-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define #{x : number} 1)
  x
  (provide x)
  (set! x 4)
  (if #t 3)
  )

(module trequire (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (require bang-tests)
  (define: y : number x))

#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module require-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (provide (all-defined))
  (require/typed x number m)  
  (+ x 3)  
  (require/typed y (number -> number) m) 
  (define: z : number (y (+ x 4))))


(module provide-type (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-type-alias top2 top)
  
  (define-typed-struct (a) container ([v : a]))
  
  (container-v (make-container 3))
  
  (provide top2 container container-v make-container)
  )

(module require-type (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (require provide-type)
  
  (let: ([x : top2 3])
        x)
  
  (define: (f [x : (container number)]) : number
    (container-v x))
  
  (f (make-container 7))
  
  )
