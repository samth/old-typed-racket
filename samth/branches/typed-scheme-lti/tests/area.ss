(module area (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct rectangle ([width : number] [height : number]))
  (define-typed-struct circle ([radius : number]))
  
  (define-type-alias shape (U rectangle circle))
  
  (define: (area [sh : shape]) : Number
    (cond [(circle? sh)
           (* (ann 3.1416 : Number) (circle-radius sh) (circle-radius sh))]
          [else
           (* (rectangle-width sh) (rectangle-height sh))])))