(module type-name-env mzscheme
  (require (lib "boundmap.ss" "syntax") (lib "list.ss")
           "tc-utils.ss"
           "base-env.ss" "type-environments.ss")
  
  
  
  (provide register-type-name
           lookup-type-name
           register-type-names
           tname-env-init-code
           initialize-type-name-env
           type-name-env-map)
  
  (define the-mapping 
     (make-module-identifier-mapping))
  
  (define (register-type-name id type)
    #;(printf "                registering type ~a~n" (syntax-e id))
    (module-identifier-mapping-put! the-mapping id type))
  
  (define (register-type-names ids types)
    (for-each register-type-name ids types))
  
  (define (lookup-type-name id k)
    #;(printf "                looking up type ~a~n" (syntax-e id))
    (module-identifier-mapping-get the-mapping id k))
    
  (define (type-name-env-map f)
    (module-identifier-mapping-map the-mapping f))
  
  (require (lib "pconvert.ss") "types.ss" "planet-requires.ss" "types-aux.ss")
  (require-for-template (lib "pconvert.ss"))
  (require-for-template (lib "shared.ss"))
  (require-for-template mzscheme "types.ss" "types-aux.ss")
  (require-for-template #;"prims.ss" (lib "match.ss"))
  (require-libs)

  (define (converter v basic sub)
    (if (union? v)
        `(Un ,@(map sub (set:elements (union-elems v))))
        (basic v)))
  
  (define (tname-env-init-code)
    (define (bound-in-this-module id) 
      (let ([binding (identifier-binding id)])
        (if (and (list? binding) (module-path-index? (car binding)))
            (let-values ([(mp base) (module-path-index-split (car binding))])
              (not mp))
            #f)))
    (define (f id ty)
        (if (bound-in-this-module id)
            #`(register-type-name #'#,id #,(datum->syntax-object #'here (print-convert ty)))
            #f))
    (parameterize ((current-print-convert-hook converter)
                   (show-sharing #f)
                   (booleans-as-true/false #f))                   
      (with-syntax ([registers (filter (lambda (x) x) (type-name-env-map f))])
        #'(begin (begin-for-syntax  . registers)))))

  (define (initialize-type-name-env)
    (for-each (lambda (nm/ty) (register-type-name (car nm/ty) (cadr nm/ty))) initial-type-names))


  
  )
