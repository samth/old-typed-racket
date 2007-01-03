(module type-env mzscheme
  (require (lib "boundmap.ss" "syntax") (lib "list.ss")
           "tc-utils.ss" "type-environments.ss"
           "base-env.ss")
  
  
  
  (provide register-type
           lookup-type
           register-types
           update-type
           env-init-code
           initialize-type-env)
  
  (define the-mapping (make-module-identifier-mapping))
  
  (define (register-type id type)
    #;(printf "registering ~a~n" (syntax-e id))
    (module-identifier-mapping-put! the-mapping id type))
  
  (define (register-types ids types)
    (for-each register-type ids types))
  
  (define (lookup-type id)
    #;(printf "looking up ~a~n" (syntax-e id))
    (module-identifier-mapping-get the-mapping id (lambda () (lookup-fail (syntax-e id)))))
  
  (define (update-type f id)
    (parameterize ([current-orig-stx id])
       (let* ([v (module-identifier-mapping-get the-mapping id (lambda () (lookup-fail (syntax-e id))))]
              [new-v (f id v)])
         (register-type id new-v))))
  
  (define (type-env-map f)
    (module-identifier-mapping-map the-mapping f))
  
  (require (lib "pconvert.ss") "types.ss" "planet-requires.ss" "types-aux.ss")
  (require-for-template (lib "pconvert.ss"))
  (require-for-template (lib "shared.ss"))
  (require-for-template mzscheme "types.ss" "types-aux.ss")
  (require-for-template "prims.ss" (lib "match.ss"))
  (require-libs)

  (define (converter v basic sub)
    (if (union? v)
        `(Un ,@(map sub (set:elements (union-elems v))))
        (basic v)))
  
  (define (env-init-code)
    (define (bound-in-this-module id) 
      (let ([binding (identifier-binding id)])
        (if (and (list? binding) (module-path-index? (car binding)))
            (let-values ([(mp base) (module-path-index-split (car binding))])
              (not mp))
            #f)))
    (define (f id ty)
        (if (bound-in-this-module id)
            #`(register-type #'#,id #,(datum->syntax-object #'here (print-convert ty)))
            #f))
    (parameterize ((current-print-convert-hook converter)
                   (show-sharing #f)
                   (booleans-as-true/false #f))                   
      (with-syntax ([registers (filter (lambda (x) x) (type-env-map f))])
        #'(begin (begin-for-syntax  . registers)))))

  (define (initialize-type-env)
    (for-each (lambda (nm/ty) (register-type (car nm/ty) (cadr nm/ty))) initial-env))

  
  
  )
