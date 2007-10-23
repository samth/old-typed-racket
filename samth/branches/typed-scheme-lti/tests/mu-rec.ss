(module mu-rec (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (define: (evenp [n : number]) : boolean
    (if (zero? n) #t (oddp (- n 1))))

  (define: (oddp [n : number]) : boolean
    (if (zero? n) #f (evenp (- n 1))))
  
  
  )