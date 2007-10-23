#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module annotation-test (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-type-alias top2 top)
  
  (define: (x) : number 23)
  
  (let: ([y : top2 x])
        y)
  
  (let: ([z : number 4])
        #{z :: top2})
  
  #{(x) :: top2}
  )