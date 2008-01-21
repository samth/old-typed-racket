

(module struct-exec "../typed-scheme.ss"
  (define-typed-struct/exec X ([a : Number] [b : Boolean]) [(lambda: ([x : X]) (+ 3 )) : (X -> Number)])
  ((make-X 1 #f))
  )
