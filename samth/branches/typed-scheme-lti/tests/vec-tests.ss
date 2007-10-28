#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module vec-tests (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (require/typed build-vector (number (number -> number) -> (Vectorof number))
                 (lib "etc.ss"))
  (define: x : (Vectorof number) (build-vector 5 (lambda: ([x : number]) 0)))
  (define: y : number (vector-ref x 1))
  #;(display x))

