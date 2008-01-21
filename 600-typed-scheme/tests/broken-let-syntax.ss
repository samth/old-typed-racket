(module broken-let-syntax (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (require-for-syntax mzscheme)
  
  (let: ([x : number 1])
        (let-syntax ([m (syntax-rules ()
                          [(_) x])])
          (m)))
  
  )