(module type-def-structs mzscheme
  (require (lib "unit.ss") (lib "struct.ss") "signatures.ss")
  
  
  (provide (all-defined))
  (define-syntax defstructs/sig/unit
    (syntax-rules (define-struct/properties)
      [(_ signame unitname (imps ...)
          def
          (define-struct/properties nm1 (flds1 ...) props #f)
          (define-struct/properties (nm par) (flds ...) () #f) ...)
       (begin
         (define-signature signame
           ((struct nm1 (flds1 ...))
            (struct nm (flds ...)) ...))
         (define-unit unitname           
           (import imps ...)
           (export signame)
           def
           (define-struct/properties nm1 (flds1 ...) props #f)
           (define-struct (nm par) (flds ...) #f) ...))]))
  
  (defstructs/sig/unit type-structs^ type-structs@
    
    (type-printer^)
    
    ;; necessary to fool the guard on custom-write, since it starts off undefined
    (define (print-type* a b c) (print-type a b c))
    
    (define-struct/properties type () 
      ([prop:custom-write print-type*])
      #f)
    
    (define-struct/properties (pair-ty type) (car cdr) () #f)
    
    
    (define-struct/properties (base-type type) (name) 
      ()
      #f)
    
    (define-struct/properties (struct-ty type) (name parent flds)
      ()
      #f)
    
    (define-struct/properties (pred-ty type) (type)
      ()
      #f)
    
    (define-struct/properties (arr type) (dom rng rest)
      ()
      #f)
    
    
    (define-struct/properties (funty type) (arities)
      ()
      #f)
    
    (define-struct/properties (value type) (v)
      ()
      #f)     
    
    (define-struct/properties (vec type) (elem)
      ()
      #f) 
    
    (define-struct/properties (union type) (elems)
      ()
      #f)
    (define-struct/properties (univ type) ()
      ()
      #f)
    (define-struct/properties (dynamic type) ()
      ()
      #f)
    
    (define-struct/properties (tvar type) (name)
      ()
      #f)
    
    (define-struct/properties (poly type) (var type)
      ()
      #f)
    
    (define-struct/properties (mu type) (var type)
      ()
      #f)
    
    (define-struct/properties (values-ty type) (types)
      ()
      #f))
  
  
  
  
  )