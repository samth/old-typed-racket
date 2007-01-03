(module type-annotation mzscheme
  
  (require "types.ss" "parse-type.ss" "tc-utils.ss")
  (provide (all-defined))
  
  (define type-label-symbol 'type-label)
  (define type-ascrip-symbol 'type-ascription)    
  
  ;; get the type annotation of this syntax
  ;; syntax -> Maybe[Type]
  (define (type-annotation stx)
    (cond [(syntax-property stx type-label-symbol) => (lambda (prop) (parse-type/id stx prop))]
          [(syntax-property stx type-ascrip-symbol) => (lambda (prop) (parse-type prop))]
          [else #f]))

  ;; get the type annotation of this identifier, otherwise error
  ;; identifier -> Type
  (define (get-type stx) 
    (parameterize
        ([current-orig-stx stx])
      (cond
        [(type-annotation stx) => (lambda (x) x)]
        [(not (syntax-original? stx)) 
         (tc-error "untyped var: ~a~n" (syntax-e stx))]
        [else
         (tc-error "no type information on variable ~a" (syntax-e stx))])))
  
  ;; get the type annotations on this list of identifiers
  ;; if not all identifiers have annotations, return the supplied inferred type
  ;; syntax-list[identifier] type -> list[type]
  (define (get-type/infer stx e-type) 
    (let/ec exit
      (map (lambda (stx)
             (cond
               [(type-annotation stx) => (lambda (x) x)]
               [else (exit (if (values-ty? e-type) (values-ty-types e-type) (list e-type)))]))
           (syntax->list stx))))
  
  )