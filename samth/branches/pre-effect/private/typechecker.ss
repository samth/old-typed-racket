(module typechecker mzscheme
  
  ;; tests needed
  
  (require (rename (lib "1.ss" "srfi") assoc* assoc)
           (lib "kerncase.ss" "syntax")
           (lib "struct.ss" "syntax")
           (lib "etc.ss")
           "subst.ss" ;; needs tests
           "utils.ss" ;; doesn't need tests
           "types.ss" ;; doesn't need tests
           "infer.ss" ;; needs tests
           "types-aux.ss" ;; maybe needs tests
           "subtype.ss" ;; has tests
           "prims.ss" ;; doesn't need tests
           "remove-intersect.ss" ;; has tests
           "planet-requires.ss" ;; doesn't need tests
           "type-env.ss" ;; maybe needs tests
           "parse-type.ss" ;; has tests
           "tc-utils.ss" ;; doesn't need tests
           "type-environments.ss" ;; doesn't need tests
           "base-env.ss" ;; doesn't need tests
           "lexical-env.ss" ;; maybe needs tests
           "type-annotation.ss" ;; has tests
           "type-name-env.ss" ;; maybe needs tests
           (lib "match.ss"))
  (require-for-template "prims.ss" (lib "match.ss"))
  
  (require-libs)
  
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))  
  
  (provide (all-defined))
  
  (require-for-template mzscheme)
  
  ;; check that e-type is compatible with ty in context of stx
  ;; otherwise, error
  ;; syntax type type -> void
  (define (check-type stx e-type ty)
    (parameterize ([current-orig-stx stx])
      (unless (subtype e-type ty)
        (tc-error "body had type ~a, variable had type ~a" e-type ty))))
  
  ;; check that expr has type ty in context of stx
  ;; syntax syntax type -> void
  (define (check-expr stx expr ty)
    (check-type stx (tc-expr expr) ty))
  
  ;; return the type of a literal value
  ;; note that the only singleton value type produced is (make-value #f)  
  ;; scheme-value -> type
  (define (tc-literal v-stx)
    ;; list[syntax] -> type
    (define (types-of-literals es)
      (apply Un (map tc-literal es)))
    (define v (syntax-e v-stx))
    (cond
      [(number? v) N]
      [(boolean? v) (if v B (make-value #f))]
      [(null? v) (make-value null) #;(make-poly '(a) (make-Listof (make-tvar 'a)))]
      [(symbol? v) Sym]
      [(string? v) String]
      [(keyword? v) Keyword]
      [(bytes? v) Bytes]
      [(list? v) (make-Listof (types-of-literals v))]
      [(vector? v) (make-vec (types-of-literals (vector->list v)))]
      [else (begin (printf "checking literal : ~a~n" v) Univ)]))
  
  (define (tc-id id)
    (cond 
      ;; bindings produced by define we trust
      #;[(syntax-property id 'defined-type-label) => (lambda (prop) (parse-type prop))]
        ;; we should check that it has a lexical binding here
        [else (lookup-type/lexical id)]))
  
  ;; typecheck an application:
  ;; arg-types: the types of the actual parameters
  ;; dom-types: the types of the function's fixed arguments
  ;; rest-type: the type of the functions's rest parameter, or #f
  ;; arg-stxs: the syntax for each actual parameter, for error reporting
  ;; [Type] [Type] Maybe[Type] [Syntax] -> Bool  
  (define (tc-args arg-types dom-types rest-type arg-stxs)
    (let loop ([args arg-types] [doms dom-types] [stxs arg-stxs])
      (cond 
        [(and (null? args) (null? doms)) #t]
        [(null? args) (tc-error "Insufficient arguments to function application, expected ~a, got ~a" 
                                (length dom-types) (length arg-types))]
        [(and (null? doms) rest-type)
         (if (subtype (car args) rest-type)
             (loop (cdr args) doms (cdr stxs))
             (tc-error/stx (car stxs) "Rest argument had wrong type, expected: ~a and got: ~a" rest-type (car args)))]
        [(null? doms)
         (tc-error "Too many arguments to function, expected ~a, got ~a" (length dom-types) (length arg-types))]
        [(subtype (car args) (car doms))
         (loop (cdr args) (cdr doms) (cdr stxs))]
        [else
         (tc-error/stx (car stxs) "Wrong function argument type, expected ~a, got ~a" (car doms) (car args))])))
  
  ;; typecheck a single lambda, with argument list and body
  ;; fixme: abstract the two cases!
  (define (tc/lambda-clause args body)
    (syntax-case args ()
      [(args ...)
       (let* ([arg-list (syntax->list #'(args ...))]
              [arg-types (map get-type arg-list)])
         (with-lexical-env/extend 
          arg-list arg-types
          (make-arr arg-types (tc-exprs (syntax->list body)))))]
      [(args ... . rest)
       (let* ([arg-list (syntax->list #'(args ...))]
              [arg-types (map get-type arg-list)]
              [rest-type (get-type #'rest)])
         (with-lexical-env/extend 
          (cons #'rest arg-list) (cons (make-Listof rest-type) arg-types)
          (make-arr arg-types (tc-exprs (syntax->list body)) rest-type)))]))
  ;; tc/mono-lambda : syntax-list syntax-list -> Funty
  ;; typecheck a sequence of case-lambda clauses
  (define (tc/mono-lambda formals bodies)
    (make-funty (map tc/lambda-clause (syntax->list formals) (syntax->list bodies))))
  ;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
  ;; tc/lambda syntax syntax-list syntax-list -> Type
  (define (tc/lambda form formals bodies)
    (if (syntax-property form 'typechecker:plambda)
        (tc/plambda form formals bodies)
        (tc/mono-lambda formals bodies)))
  ;; tc/plambda syntax syntax-list syntax-list -> Poly
  ;; formals and bodies must by syntax-lists
  (define (tc/plambda form formals bodies)
    (with-syntax ([tvars (syntax-property form 'typechecker:plambda)])
      (let* ([literal-tvars (map syntax-e (syntax->list #'tvars))]
             [new-tvars (map make-tvar literal-tvars)]
             [ty (parameterize ([current-tvars (extend-env literal-tvars new-tvars (current-tvars))])
                   (tc/mono-lambda formals bodies))])
        (make-poly literal-tvars ty))))
  
  ;; if typechecking 
  (define (tc/if-onearm tst body) (tc/if-twoarm tst body (syntax/loc body (#%app void))))
  
  (define (tc/if-twoarm tst thn els)
    (define ((tc/update f) var t expr)
      (let/ec return
        (with-update-type/lexical (lambda (_ old) 
                                    (let ([new-t (f old t)])
                                      (if (subtype new-t (Un))
                                          (return (Un))
                                          new-t)))
                                  var 
                                  (tc-expr expr))))
    (define tc/remove (tc/update remove))
    ;; (define tc/intersect (tc/update intersect)) old version
    (define tc/intersect (tc/update restrict))
    (define tc/replace (tc/update (lambda (old t) t)))
    ;; check that tst is well-typed
    (tc-expr tst)
    (kernel-syntax-case tst #f
      [(#%app p v) 
       (identifier? #'v)
       (match (tc-expr #'p)
         [($ pred-ty t)
          #;(printf "using pred ~a~n" t)          
          (Un (tc/intersect #'v t thn) (tc/remove #'v t els))]
         [_ (Un (tc-expr thn) (tc-expr els))])]
      [i (identifier? #'i)
         (Un (tc/remove #'i (make-value #f) thn)                     
             (tc/replace #'i (make-value #f) els))]
      [_ (Un (tc-expr thn) (tc-expr els))]))
  
  
  ;; type check form in the current type environment
  ;; if there is a type error in form, or if it has the wrong annotation, error
  ;; otherwise, produce the type of form
  ;; syntax[expr] -> type
  (define (tc-expr form)
    (define ty-ann (type-annotation form))    
    ;; do the actual typechecking of form
    ;; internal-tc-expr : syntax -> Type
    (define (internal-tc-expr form)
      (kernel-syntax-case* form #f (values apply letrec-syntaxes+values) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
        ;; data
        [(#%datum . val) (tc-literal #'val)]
        [(quote val)  (tc-literal #'val)]
        ;; lambda
        [(lambda formals . body)
         (tc/lambda form #'(formals) #'(body))]         
        [(case-lambda [formals . body] ...)
         (tc/lambda form #'(formals ...) #'(body ...))]      
        ;; let
        [(let-values ([(name ...) expr ] ...) . body)
         (let* (;; a list of each name clause
                [names (syntax->list #'((name ...) ...))]
                ;; all the trailing expressions - the ones actually bound to the names
                [exprs (syntax->list #'(expr ...))]
                ;; the types of the expr0s
                [inferred-types (map tc-expr exprs)]
                ;; the annotated types of the name (possibly using the inferred types)
                [types (map get-type/infer names inferred-types)]
                ;; just for error reporting
                [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
           ;; extend the lexical environment for checking the body
           (with-lexical-env/extend 
            ;; the list of lists of name
            (map syntax->list names) 
            ;; the types
            types
            (for-each (lambda (stx e-type t) (check-type stx e-type t))
                      clauses
                      inferred-types 
                      (map list->values-ty types))
            (tc-exprs (syntax->list #'body))))]
        [(letrec-values ([(name ...) expr] ...) . body)
         (let* ([names (map syntax->list (syntax->list #'((name ...) ...)))]
                [exprs (syntax->list #'(expr ...))]
                [types (map (lambda (l) (map get-type l)) names)]                
                ;; just for error reporting
                [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])            
           (with-lexical-env/extend
            names
            types
            (for-each (lambda (stx e t) (check-expr stx e t))
                      clauses
                      exprs
                      (map list->values-ty types))
            (tc-exprs (syntax->list #'body))))]
        
        ;; mutation!
        [(set! id val)
         (let* ([id-t (tc-id #'id)]
                [val-t (tc-expr #'val)])
           (unless (subtype val-t id-t)
             (tc-error "Mutation only allowed with compatible types: ~a is not a subtype of ~a" val-t id-t))
           Void)]
        
        ;; variable reference
        [(#%top . id) #;(lookup-type #'id)
                      (tc-error "#%top should never happen")]
        [x (identifier? #'x) (tc-id #'x)]                 
        
        [(#%variable-reference . _)
         (tc-error "do not use #%variable-reference")]
        ;; application
        ;; special case for `values'
        [(#%app values . args)
         (let ([tys (map tc-expr (syntax->list #'args))])
           (list->values-ty tys))]
        ;; special case for `apply'
        [(#%app apply f . args)
         (let* ([f-ty (tc-expr #'f)]
                [arg-tys0 (map tc-expr (syntax->list #'args))])
           (define (split l)
             (let loop ([l l] [acc '()])
               (if (null? (cdr l))
                   (values (reverse acc) (car l))
                   (loop (cdr l) (cons (car l) acc)))))
           (let-values ([(arg-tys tail-ty) (split arg-tys0)])
             (match f-ty
               [($ funty (($ arr doms rngs rests) ..1))
                (let loop ([doms doms] [rngs rngs] [rests rests])
                  (cond [(null? doms) (tc-error "no function domain matched - domains were: ~a arguments were ~a" doms arg-tys0)]
                        [(and (subtypes arg-tys (car doms)) (subtype tail-ty (make-Listof (car rests))))
                         (car rngs)]
                        [else (loop (cdr doms) (cdr rngs) (cdr rests))]))]                      
               [($ poly _ ($ funty _))
                (tc-error "polymorphic functions not supported with apply")]
               [else (tc-error "~a is not a function type" f-ty)])))]
        [(#%app f args ...)
         (let* ([ftype (tc-expr #'f)]
                [argtypes (map tc-expr (syntax->list #'(args ...)))])
           (match ftype
             [($ pred-ty _) 
              (if (= 1 (length argtypes))
                  B
                  (tc-error "wrong number of arguments to predicate ~a" (syntax-e #'f)))]
             [($ funty (($ arr doms rngs rests) ..1))
              #;(printf "ftype is ~a~n" ftype)
              (if (= 1 (length doms))
                  (begin 
                    (tc-args argtypes (car doms) (car rests) (syntax->list #'(args ...)))
                    (car rngs))
                  (let loop ([doms* doms] [rngs rngs] [rests rests])
                    (cond [(null? doms*) 
                           (tc-error "no function domain matched - domains were: ~a arguments were ~a" doms argtypes)]
                          [(subtypes/varargs argtypes (car doms*) (car rests)) (car rngs)]
                          [else (loop (cdr doms*) (cdr rngs) (cdr rests))])))]
             [($ poly vars ($ funty (($ arr doms rngs #f) ...)))
              (for-each (lambda (x) (unless (not (poly? x))                                      
                                      (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                        argtypes)
              (let* ([fresh-names (map gensym vars)]
                     [fresh-vars (map make-tvar fresh-names)]
                     [fresh-doms (map (lambda (argl) (map (lambda (t) (subst-all (map list vars fresh-vars) t)) argl)) doms)]
                     [fresh-rngs (map (lambda (l) (subst-all (map list vars fresh-vars) l)) rngs)])
                (let loop ([doms* fresh-doms] [rngs* fresh-rngs])
                  (cond [(null? doms*)
                         (tc-error "no polymorphic function domain matched - domains were: ~a arguments were ~a" doms argtypes)]
                        [(and (= (length (car doms*))
                                 (length argtypes))
                              (unify (map list (car doms*) argtypes)))
                         => (lambda (substitution) 
                              (subst-all (map list fresh-names (map make-tvar vars))
                                         (subst-all substitution (car rngs*))))]
                        [else (loop (cdr doms*) (cdr rngs*))])))]
             [else (tc-error "~a is not a function type" ftype)]))]
        ;; if
        [(if tst body) (tc/if-onearm #'tst #'body)]               
        [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els)]                          
        
        ;; syntax
        [(letrec-syntaxes+values stxs vals . body)
         (tc-expr (syntax/loc form (letrec-values vals . body)))]
        
        ;; begin
        [(begin e) (tc-expr #'e )]
        [(begin e es ...)
         (begin (tc-expr #'e)
                (tc-expr #'(begin es ...)))]
        #;[(begin) Void] ;; impossible
        
        [(begin0 e) (tc-expr #'e )]
        [(begin0 e es ...)
         (begin0 (tc-expr #'e)
                 (tc-expr #'(begin es ...)))]
        #;[(begin0) Void] ;; impossible
        
        ;; other
        [_ (tc-error "cannot typecheck unknown form : ~a~n" (syntax-object->datum form))]))
    
    (parameterize ([current-orig-stx form])                   
      (unless (syntax? form) 
        (tc-error "bad form input to tc-expr"))
      (let ([result (internal-tc-expr form)])
        (cond [(and ty-ann (subtype result ty-ann)) ty-ann]
              [ty-ann (tc-error "expression had type ~a, but was annotated with type ~a" result ty-ann)]
              [else result])))
    )
  
  ;; type-check a list of exprs, producing the type of the last one.
  ;; if the list is empty, the type is Void.
  ;; list[syntax[expr]] -> type
  (define (tc-exprs exprs)
    (cond [(null? exprs) Void]
          [(null? (cdr exprs)) (tc-expr (car exprs))]
          [else (tc-expr (car exprs))
                (tc-exprs (cdr exprs))]))
  
  ;; FIXME - merge these functions
  (define (tc/poly-struct vars nm flds tys)
    (with-syntax*
        ([(nm parent) (syntax-case nm ()
                        [nm (identifier? #'nm) #'(nm #f)]
                        [(nm par) #'(nm par)])]
         [(_ maker pred . getters) (build-struct-names #'nm flds #f #t)])
      (let* ([name (syntax-e #'nm)]
             [tvars (map syntax-e vars)]
             [new-tvars (map make-tvar tvars)]
             [parent (if (syntax-e #'parent) (parse-type #'parent) #f)])
        (parameterize ([current-tvars (extend-env tvars new-tvars (current-tvars))])
          (let*
              ([name-tvar (make-tvar name)]
               ;; parse the types with an env so we can check if the type is recursive
               [types (parameterize 
                          ([current-tvars 
                            (extend-env (list name) (list (make-poly tvars name-tvar)) (current-tvars))])
                        (map parse-type tys))]
               [rec? (ormap (lambda (s) (set:member? name s)) (map fv types))]                      
               ;; register the type name for the re-parse
               [_ (if rec? (register-type-name #'nm (make-poly tvars name-tvar)))]
               ;; now we reparse to get the correct types
               [types (map parse-type tys)]
               [parent-field-types (if parent 
                                       (struct-ty-flds (if (poly? parent)
                                                           (subst-all (map list (poly-var parent) new-tvars) 
                                                                      (poly-type parent))
                                                           parent))
                                       '())]
               [fld-types (append parent-field-types types)]
               [sty (make-struct-ty name parent fld-types)]
               [sty (if rec?
                        (make-mu name sty)
                        sty)]
               [psty (make-poly* tvars sty)]
               [external-fld-types/no-parent (map (lambda (t) (subst name sty t)) types)]
               [external-fld-types (map (lambda (t) (subst name sty t)) fld-types)]
               [result-tvars (filter (lambda (x) (not (member x (fv/list sty)))) tvars)]
               [result-sty (if (null? result-tvars) sty
                               (make-poly result-tvars sty))])                      
            #;(printf "added constructor of type ~a~n" (make-poly tvars (->* external-fld-types result-sty)))
            ;; finally, register the correct type name
            (register-type-name #'nm psty)
            (register-type #'maker (make-poly tvars (->* external-fld-types sty)))
            (register-types (syntax->list #'getters)
                            (map (lambda (t) (make-poly tvars (->* (list sty) t))) external-fld-types/no-parent))
            (register-type #'pred (make-pred-ty psty)))))))
  
  (define (tc/struct nm flds tys)
    (with-syntax*
        ([(nm parent) (syntax-case nm ()
                        [nm (identifier? #'nm) #'(nm #f)]
                        [(nm par) #'(nm par)])]
         [(_ maker pred . getters) (build-struct-names #'nm flds #f #t)])
      (let* ([name (syntax-e #'nm)]
             [name-tvar (make-tvar name)]
             [parent (if (syntax-e #'parent) (parse-type #'parent) #f)]
             [types (parameterize 
                        ([current-tvars 
                          (extend-env (list name) (list name-tvar) (current-tvars))])
                      (map parse-type tys))]
             [rec? (ormap (lambda (s) (set:member? name-tvar s)) (map fv types))]                                          
             [parent-field-types (if parent (struct-ty-flds parent) '())]
             [fld-types (append parent-field-types types)]
             [sty (make-struct-ty name parent fld-types)]
             [sty (if rec?
                      (make-mu name sty)
                      sty)]
             [external-fld-types/no-parent (map (lambda (t) (subst name sty t)) types)]
             [external-fld-types (map (lambda (t) (subst name sty t)) fld-types)])
        #;(printf "~a~n ~a~n" sty (->* external-fld-types sty))
        (register-type-name #'nm sty)
        (register-type #'maker (->* external-fld-types sty))
        (register-types (syntax->list #'getters)
                        (map (lambda (t) (->* (list sty) t)) external-fld-types/no-parent))
        (register-type #'pred (make-pred-ty sty)))))
  
  ;; produce the appropriate type of a list of types
  ;; that is - if there is exactly one type, just produce it, otherwise produce a values-ty
  ;; list[type] -> type
  (define (list->values-ty l)
    (if (= 1 (length l)) (car l) (make-values-ty l)))
  
  (define (tc-toplevel/pass1 form)
    (parameterize ([current-orig-stx form])
      (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal require/typed-internal : tc/check)
        ;; forms that are handled in other ways
        [stx 
         (syntax-property form 'typechecker:ignore)
         (void)]
        
        ;; directives to the typechecker
        [(begin0 (require/typed-internal nm ty))
         (register-type #'nm (parse-type #'ty))]
        [(begin0 (define-type-alias-internal (nm . vars) ty))
         (register-type-name #'nm (parse-type #'(All vars ty)))]
        [(begin0 (define-type-alias-internal nm ty))
         (register-type-name #'nm (parse-type #'ty))]
        
        [(begin0 (define-typed-struct-internal nm ([fld : ty] ...)))
         (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]
        [(begin0 (define-typed-struct-internal (vars ...) nm ([fld : ty] ...)))
         (tc/poly-struct (syntax->list #'(vars ...)) #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]

        [(begin0 (define-typed-internal nm [variant (fld ty) ...] ...))
         (void)]
        
        [(define-values (var ...) expr)
         (andmap (lambda (s) (syntax-property s 'type-label)) (syntax->list #'(var ...)))
         (let* ([vars (syntax->list #'(var ...))]
                [ts (map get-type vars)])
           (for-each register-type vars ts))]
        [(define-values . _)
         (tc-error "Untyped definition")]
        [_ (void)]
        
        ;; these forms should be ignored
        [(require . _) (void)]
        [(require-for-syntax . _) (void)]
        [(require-for-template . _) (void)]
        [(provide . _) (void)]
        [(define-syntaxes . _) (void)]
        
        ;; otherwise, the form was just an expression
        [_ (begin
             (tc-expr form)
             #;cenv)])))
  
  ;; typecheck the expressions of a module-top-level form
  ;; no side-effects
  ;; syntax -> void
  (define (tc-toplevel/pass2 form)
    (parameterize ([current-orig-stx form])
      (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal require/typed-internal : tc/check)
        ;; these forms we have been instructed to ignore
        [stx 
         (syntax-property form 'typechecker:ignore)
         (void)]
        
        ;; these forms should always be ignored
        [(require . _) (void)]
        [(require-for-syntax . _) (void)]
        [(require-for-template . _) (void)]
        [(provide . _) (void)]
        [(define-syntaxes . _) (void)]
        
        ;; these forms are handled in pass1
        [(begin0 (require/typed-internal . rest)) (void)]
        [(begin0 (define-type-alias-internal . rest)) (void)]
        [(begin0 (define-typed-struct-internal . rest)) (void)]        
        [(begin0 (define-type-internal . rest)) (void)]        
        
        ;; definitions just need to typecheck their bodies
        [(define-values (var ...) expr)
         (let* ([vars (syntax->list #'(var ...))]
                [ts (map get-type vars)])
           (check-expr form #'expr (list->values-ty ts)))]
        
        ;; otherwise, the form was just an expression
        [_ (tc-expr form)])))
  
  
  ;; type check a list of module-level forms
  ;; produce code to add the definitions in this module to the global table of module-level forms
  ;; syntax-list -> syntax
  (define (type-check forms)
    (let ([forms (syntax->list forms)])
      ;; install defined names and types in the environment
      (for-each tc-toplevel/pass1 forms)
      ;; typecheck the expressions
      (for-each tc-toplevel/pass2 forms))
    #`(begin
        #,(env-init-code)
        #,(tname-env-init-code)))
  
  ;(trace subtype-of)
  ;(trace check-expr)
  ;(trace tc-expr)
  ;(trace intersect-ty)
  ;(trace remove-ty)
  ;(trace all-in)
  ;(trace symbolic-identifier=?)
  
  ;(trace parse-type)
  
  
  )