#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module scratch (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
#;  (define: l : (Listof number) null)
  
#;  (cons 1 l)
  
#;  (car (cons 2 (cons 1 l)))
  
#;  #{(cond [(null? l) 0]
          [(pair? l) (car l)]) :: number}
  
  (define-type Tree
    [Leaf (val number)]
    [Node (left Tree) (right Tree)])
  
  #;(define: (fib [n : number]) : number
    (cond [(= n 1) n]
          [else (+ (fib (sub1 n) (sub1 (sub1 n))))]))
  
  
  )
