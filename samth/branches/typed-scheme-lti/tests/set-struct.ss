(module set-struct "../typed-scheme.ss"
  
  (define-typed-struct A ([x : Number] [y : Boolean]))
  
  (define: (f [a : A]) : Number
    (set-A-x! a 4)
    (set-A-y! a #f)
    (+ 4 (A-x a)))
  
  (display (f (make-A 11 #t)))
  )