#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module if-splitting-test (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (define: l : (list-of number)
    (cons 1 (cons 2 (cons 3 #{'() : (list-of number)}))))
  
  (define: (g [x : number]) : number
    (cond [(memv x l) => car]
          [else 0]))
  
  )