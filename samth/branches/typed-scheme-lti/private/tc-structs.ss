(module tc-structs mzscheme
  
  (require (lib "struct.ss" "syntax")
           (lib "etc.ss")
           "type-rep.ss" ;; doesn't need tests
           "type-effect-convenience.ss" ;; maybe needs tests
           "type-env.ss" ;; maybe needs tests
           "parse-type.ss" ;; has tests
           "type-environments.ss" ;; doesn't need tests
           "type-name-env.ss" ;; maybe needs tests
           "utils.ss"
           "union.ss"
           (lib "trace.ss")
           (lib "kw.ss")
           (lib "plt-match.ss"))

  
  (require-for-template mzscheme)
  
  (provide (all-defined))
  
  ;; parse name field of struct, determining whether a parent struct was specified
  ;; syntax -> (values identifier Option[Type] List[Types] Symbol Type)
  (define (parse-parent nm/par)
    (syntax-case nm/par ()
      [nm (identifier? #'nm) (values #'nm #f (syntax-e #'nm) (make-F (syntax-e #'nm)))]
      [(nm par) (let ([parent (parse-type #'par)])
                  (values #'nm parent (syntax-e #'nm) (make-F (syntax-e #'nm))))]))
  
  ;; generate struct names given type name and field names
  ;; generate setters if setters? is true
  ;; all have syntax loc of name
  ;; identifier listof[identifier] boolean -> (values identifier identifier list[identifier] Option[list[identifier]])
  (define (struct-names nm flds setters?)
    (define (split l)
      (let loop ([l l] [getters '()] [setters '()])
        (if (null? l)
            (values (reverse getters) (reverse setters))
            (loop (cddr l) (cons (car l) getters) (cons (cadr l) setters)))))
    (match (build-struct-names nm flds #f (not setters?) nm)
      [(list _ maker pred getters/setters ...) 
       (if setters?
           (let-values ([(getters setters) (split getters/setters)])
             (values maker pred getters setters))
           (values maker pred getters/setters #f))]))
  
  ;; parse tys in an environment where name is mapped to dummy-type
  ;; produce the parsed types, and a boolean representing whether name was used
  ;; symbol type list[syntax] -> list[type] boolean
  (define (parse-types/rec name dummy-type tys)
    (let* (;; parse the types with an env so we can check if the type is recursive
           [types (parameterize 
                      ([current-tvars 
                        (extend-env (list name) (list dummy-type) (current-tvars))])
                    (map parse-type tys))]
           ;; if the name of the type is in the free variables, then there was a self reference
           [rec? (ormap (lambda (s) (member name s)) (map fv types))])
      (values types rec?)))  
  
  ;; gets the fields of the parent type, if they exist
  ;; Option[Struct-Ty] -> Listof[Type]
  (define (get-parent-flds p)
    (match p
      [(Struct: _ _ flds _) flds]
      [#f null]))
  
  
  ;; construct all the various types for structs, and then register the approriate names
  ;; identifier listof[identifier] type listof[Type] listof[Type] boolean -> Type listof[Type] listof[Type]
  (define (mk/register-sty nm flds parent parent-field-types types rec? wrapper setters? proc-ty)
    (let* ([name (syntax-e nm)]
           [fld-types (append parent-field-types types)]
           [sty-initial (make-Struct name parent fld-types proc-ty)]
           [sty (if rec? (make-Mu name sty-initial) sty-initial)]
           [external-fld-types/no-parent (map (lambda (t) (subst name sty t)) types)]
           [external-fld-types (map (lambda (t) (subst name sty t)) fld-types)])
      (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters? #:wrapper wrapper)))
  
  ;; generate names, and register the approriate types give field types and structure type
  ;; optionally wrap things
  ;; identifier Type Listof[identifer] Listof[Type] Listof[Type] #:wrapper (Type -> Type) #:maker identifier
  (define/kw (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters?
                                    #:key
                                    [wrapper (lambda (x) x)]
                                    [maker* #:maker #f])
    ;; create the approriate names that define-struct will bind
    (define-values (maker pred getters setters) (struct-names nm flds setters?))
    ;; register the type name
    (register-type-name nm (wrapper sty))
    ;; register the various function types
    (register-type (or maker* maker) (wrapper (->* external-fld-types sty)))
    (register-types getters
                    (map (lambda (t) (wrapper (->* (list sty) t))) external-fld-types/no-parent))
    (when setters?    
      #;(printf "setters: ~a~n" (syntax-object->datum setters))
      (register-types setters
                      (map (lambda (t) (wrapper (->* (list sty t) -Void))) external-fld-types/no-parent)))
    (register-type pred (make-pred-ty (wrapper sty))))
  
  ;; check and register types for a polymorphic define struct
  ;; tc/poly-struct : Listof[identifier] (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
  (define (tc/poly-struct vars nm/par flds tys)
    ;; parent field types can't actually be determined here
    (define-values (nm parent name name-tvar) (parse-parent nm/par))
    ;; create type variables for the new type parameters
    (define tvars (map syntax-e vars))
    (define new-tvars (map make-F tvars))
    ;; parse the types, and determine if this is recursive
    (define-values (types rec?)
      ;; add the type parameters of this structure to the tvar env
      (parameterize ([current-tvars (extend-env tvars new-tvars (current-tvars))])
        ;; parse the first time, with the struct names bound to a dummy polymorphic type 
        ;; (that has the right number of type parameters)
        (let*-values ([(types-init rec?) (parse-types/rec name (make-Poly tvars name-tvar) tys)])
          ;; register the type name for the re-parse, if necessary
          (if rec? (register-type-name nm (make-Poly tvars name-tvar)))
          ;; now we reparse to get the correct types, if necessary
          (values (if rec? (map parse-type tys) types-init) rec?))))
    ;; instantiate the parent if necessary, with new-tvars
    (define concrete-parent 
      (if (Poly? parent)
          (instantiate-poly parent new-tvars)
          parent))
    ;; get the fields of the parent, if it exists
    (define parent-field-types (get-parent-flds concrete-parent))
    ;; create the actual structure type, and the types of the fields
    ;; that the outside world will see
    ;; then register them
    (mk/register-sty nm flds parent parent-field-types types rec?
                     ;; wrap everything in the approriate forall
                     (lambda (t) (make-Poly tvars t))
                     ;; no setters
                     #f
                     ;; not a function
                     #f))
  
  
  ;; typecheck a non-polymophic struct and register the approriate types
  ;; tc/struct : (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
  (define tc/struct    
    (opt-lambda (nm/par flds tys [proc-ty #f])
      ;; get the parent info and create some types and type variables
      (define-values (nm parent name name-tvar) (parse-parent nm/par))
      ;; parse the field types, and determine if the type is recursive
      (define-values (types rec?) (parse-types/rec name name-tvar tys))
      (define-values (proc-ty-parsed proc-rec?) 
        (if proc-ty
            (parse-types/rec name name-tvar (list proc-ty))
            (values (list #f) #f)))
      ;; create the actual structure type, and the types of the fields
      ;; that the outside world will see
      (mk/register-sty nm flds parent (get-parent-flds parent) types (or rec? proc-rec?)
                       ;; wrapper does nothing
                       (lambda (t) t)
                       ;; generate setters
                       #t
                       ;; procedure
                       (car proc-ty-parsed))))
  
  ;; register a struct type
  ;; convenience function for built-in structs
  ;; tc/builtin-struct : identifier identifier Listof[identifier] Listof[Type] Listof[Type] -> void
  (define (tc/builtin-struct nm parent flds tys parent-tys)
    (let ([parent* (if parent (lookup-type-name parent) #f)])
      (mk/register-sty nm flds parent* parent-tys tys #f (lambda (t) t) #t #f)))
  
  
  ;; parent-nm is an identifier with the name of the defined type
  ;; variants is (list id id (list (cons id unparsed-type))) - first id is name of variant, second is name of maker, 
  ;;     list is name of field w/ type
  ;; top-pred is an identifier
  ;; produces void
  (define (tc/define-type parent-nm top-pred variants)
    ;; the symbol and type variable used for parsing
    (define parent-sym (syntax-e parent-nm))
    (define parent-tvar (make-F parent-sym))
    
    ;; create the initial struct type, which contains type variables
    (define (mk-initial-variant nm fld-tys-stx)
      ;; parse the types (recursiveness doesn't matter)
      (define-values (fld-tys _) (parse-types/rec parent-sym parent-tvar fld-tys-stx))     
      (make-Struct (syntax-e nm) #f fld-tys #f))
    
    ;; create the union type that is the total type
    (define (mk-un-ty parent-sym variant-struct-tys)
      (make-Mu parent-sym (apply Un variant-struct-tys)))     
    
    ;; generate the names and call mk-variant
    (define (mk-variant nm maker-name fld-names un-ty variant-struct-ty parent-nm)
      ;; construct the actual type of this variant
      (define variant-ty (subst parent-nm un-ty variant-struct-ty))
      ;; the fields of this variant
      (match-define (Struct: _ _ fld-types _) variant-ty)
      ;; register all the types (with custon maker name)
      (register-struct-types nm variant-ty fld-names fld-types fld-types #f #:maker maker-name))
    
    ;; all the names
    (define variant-names (map car variants))
    (define variant-makers (map cadr variants))
    (define variant-flds (map caddr variants))    
    ;; create the initial variants, which don't have the parent substituted in
    (define variant-struct-tys (map (lambda (n flds) (mk-initial-variant n (map car flds))) variant-names variant-flds))
    ;; just the names of each variant's fields
    (define variant-fld-names (map (lambda (x) (map cdr x)) variant-flds))
    
    ;; the type of the parent
    (define un-ty (mk-un-ty parent-sym variant-struct-tys))
    
    ;; register the types for the parent
    (register-type top-pred (make-pred-ty un-ty))
    (register-type-name parent-nm un-ty)
    
    ;; construct all the variants, and register the appropriate names
    (for-each (lambda (nm mk fld-names sty) (mk-variant nm mk fld-names un-ty sty parent-sym))
              variant-names variant-makers variant-fld-names variant-struct-tys))
  
  
    
  )