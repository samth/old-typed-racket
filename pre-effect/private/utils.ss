(module utils mzscheme
  (provide with-syntax* syntax-map)
  
  (define-syntax (with-syntax* stx)
    (syntax-case stx ()
      [(_ (cl) body ...) #'(with-syntax (cl) body ...)]
      [(_ (cl cls ...) body ...)
       #'(with-syntax (cl) (with-syntax* (cls ...) body ...))]
      ))

  (define (syntax-map f stxl)
    (map f (syntax->list stxl)))
  
  )