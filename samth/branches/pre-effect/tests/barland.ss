#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module barland (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (define-type-alias set (top -> top))
  
  (define: (autos [elt : top]) : top (memq elt #{'(vw saab bmw audi) : (list-of top)}))

  (define: (element-of? [elt : top] [s : set]) : top (s elt))
  
  (define: (evens [elt : top]) : top (and (number? elt) (even? elt)))
  
  (define-typed-struct pr ([fst : top] [snd : top]))

  #;(define: (length=2? [any : top]) : boolean
    (and (pair? any)
         (pair? (cdr any))
         (empty? (cdr (cdr any)))))

  (define: (cartesian-product [A : set] [B : set]) : set
    (lambda: ([elt : top])
      (and (pr? elt)
           (element-of? (pr-fst elt) A)
           (element-of? (pr-snd elt) B))))
  
  
  
  (define: evenEuroCars : set (cartesian-product evens autos))
  #;(display (element-of? (make-pr 4 'bmw) evenEuroCars)) ; = #t
  #;(display (element-of? (make-pr 'bmw 4) evenEuroCars)) ; = #f
  
  )