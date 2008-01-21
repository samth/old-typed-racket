(module pair-test (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (define: x : (number . boolean) (cons 3 #f))
  
  (define: y : number (car x))
  
  (define: z : boolean (cdr x))
  
  )