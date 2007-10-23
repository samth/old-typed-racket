(module type-name-env mzscheme
  (require (lib "boundmap.ss" "syntax") (lib "list.ss")
           "tc-utils.ss")
      
  (provide register-type-name
           lookup-type-name
           register-type-names
           type-name-env-map)
  
  ;; a mapping from id -> type (where id is the name of the type)
  (define the-mapping 
     (make-module-identifier-mapping))
  
  ;; add a name to the mapping
  ;; identifier Type -> void
  (define (register-type-name id type)
    #;(printf "registering type ~a~n~a~n" (syntax-e id) id)
    (module-identifier-mapping-put! the-mapping id type))
  
  ;; add a bunch of names to the mapping
  ;; listof[identifier] listof[type] -> void
  (define (register-type-names ids types)
    (for-each register-type-name ids types))
  
  ;; given an identifier, return the type associated with it
  ;; optional argument is failure continuation - default calls lookup-fail
  ;; identifier (-> error) -> type 
  (define lookup-type-name
    (case-lambda
      [(id) (lookup-type-name id (lambda () (lookup-fail (syntax-e id))))]
      [(id k) (let ([v (module-identifier-mapping-get the-mapping id k)])
                (begin
                  (add-type-name-reference id)
                  v))]))
    
  ;; map over the-mapping, producing a list
  ;; (id type -> T) -> listof[T]
  (define (type-name-env-map f)
    (module-identifier-mapping-map the-mapping f))
  

  
  )
