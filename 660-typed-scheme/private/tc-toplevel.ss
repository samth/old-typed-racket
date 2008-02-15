#lang scheme/unit


(require syntax/kerncase
         mzlib/etc
         mzlib/plt-match
         "signatures.ss"
         "tc-structs.ss"
         "type-utils.ss"
         "utils.ss" ;; doesn't need tests
         "type-effect-convenience.ss" ;; maybe needs tests
         "internal-forms.ss" ;; doesn't need tests
         "type-env.ss" ;; maybe needs tests
         "parse-type.ss" ;; has tests
         "tc-utils.ss" ;; doesn't need tests
         "type-annotation.ss" ;; has tests
         "type-name-env.ss" ;; maybe needs tests
         "init-envs.ss"
         "mutated-vars.ss"
         "def-binding.ss"
         "provide-handling.ss"
         "type-alias-env.ss"
         "type-contract.ss"
         
         (for-template
          "internal-forms.ss"
          "tc-utils.ss"
          (lib "contract.ss")
          scheme/base))

(import tc-expr^ check-subforms^)
(export typechecker^)

(define (tc-toplevel/pass1 form)
  ;(printf "form-top: ~a~n" form)
  ;; first, find the mutated variables:
  (find-mutated-vars form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                   define-typed-struct/exec-internal :-internal assert-predicate-internal
                                   require/typed-internal : void)
      ;; forms that are handled in other ways
      [stx 
       (or (syntax-property form 'typechecker:ignore) 
           (syntax-property form 'typechecker:ignore-some))
       (list)]
      
      ;; type aliases have already been handled by an earlier pass
      [(#%plain-app void (quote-syntax (define-type-alias-internal nm ty)))
       (list)]
      
      ;; require/typed
      [(#%plain-app void (quote-syntax (require/typed-internal nm ty)))
       (register-type #'nm (parse-type #'ty))]
      
      ;; define-typed-struct
      [(#%plain-app void (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...))))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]
      [(#%plain-app void (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...) #:maker m)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #:maker #'m)]
      ;; define-typed-struct w/ polymorphism
      [(#%plain-app void (quote-syntax (define-typed-struct-internal (vars ...) nm ([fld : ty] ...))))
       (tc/poly-struct (syntax->list #'(vars ...)) #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]    
      
      ;; executable structs - this is a big hack
      [(#%plain-app void (quote-syntax (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #'proc-ty)]
      
      ;; predicate assertion - needed for define-type b/c or doesn't work
      [(#%plain-app void (quote-syntax (assert-predicate-internal ty pred)))
       (register-type #'pred (make-pred-ty (parse-type #'ty)))]
      
      ;; top-level type annotation
      [(#%plain-app void (quote-syntax (:-internal id ty)))
       (identifier? #'id)
       (register-type/undefined #'id (parse-type #'ty))]
      
      ;; values definitions
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))])
         (cond
           ;; if all the variables have types, we stick them into the environment
           [(andmap (lambda (s) (syntax-property s 'type-label)) vars)        
            (let ([ts (map get-type vars)])
              (for-each register-type vars ts)
              (map make-def-binding ts vars))]
           ;; if this already had an annotation, we just construct the binding reps
           [(andmap (lambda (s) (lookup-type s (lambda () #f))) vars)
            (for-each finish-register-type vars)
            (map (lambda (s) (make-def-binding s (lookup-type s))) vars)]
           ;; special case to infer types for top level defines - should handle the multiple values case here
           [(and (= 1 (length vars)) 
                 (with-handlers ([exn:fail? (lambda _ #f)]) (tc-expr #'expr)))
            => (match-lambda 
                 [(tc-result: t)
                  (register-type (car vars) t)
                  (list (car vars) t)])]
           [else
            (tc-error "Untyped definition : ~a" (map syntax-e vars))]))]
      
      ;; to handle the top-level, we have to recur into begins
      [(begin . rest)
       (apply append (filter list? (map tc-toplevel/pass1 (syntax->list #'rest))))]
      
      ;; define-syntaxes just get noted
      [(define-syntaxes (var ...) . rest)
       (andmap identifier? (syntax->list #'(var ...)))
       (map make-def-stx-binding (syntax->list #'(var ...)))]
      
      ;; otherwise, do nothing in this pass
      ;; handles expressions, provides, requires, etc and whatnot
      [_ (list)])))





;; typecheck the expressions of a module-top-level form
;; no side-effects
;; syntax -> void
(define (tc-toplevel/pass2 form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                   require/typed-internal void)
      ;; these forms we have been instructed to ignore
      [stx 
       (syntax-property form 'typechecker:ignore)
       (void)]
      
      ;; this is a form that we mostly ignore, but we check some interior parts
      [stx 
       (syntax-property form 'typechecker:ignore-some)
       (check-subforms/ignore form)]
      
      ;; these forms should always be ignored
      [(#%require . _) (void)]
      [(#%provide . _) (void)]
      [(define-syntaxes . _) (void)]
      [(define-values-for-syntax . _) (void)]
      
      ;; these forms are handled in pass1
      [(#%plain-app void (quote-syntax (require/typed-internal . rest))) (void)]
      [(#%plain-app void (quote-syntax (define-type-alias-internal . rest))) (void)]
      [(#%plain-app void (quote-syntax (define-typed-struct-internal . rest))) (void)]          
      
      ;; definitions just need to typecheck their bodies
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))]
              [ts (map lookup-type vars)])
         (tc-expr/check #'expr (list->values-ty ts)))
       (void)]
      
      ;; to handle the top-level, we have to recur into begins
      [(begin) (void)]
      [(begin . rest)
       (let loop ([l (syntax->list #'rest)])
         (if (null? (cdr l))
             (tc-toplevel/pass2 (car l))
             (begin (tc-toplevel/pass2 (car l))
                    (loop (cdr l)))))]
      
      ;; otherwise, the form was just an expression
      [_ (tc-expr form)])))



;; new implementation of type-check
(define-syntax-rule (internal-syntax-pred nm)
  (lambda (form)
    (kernel-syntax-case* form #f 
      (nm void)
      [(#%plain-app void (quote-syntax (nm . rest))) #t]
      [_ #f])))

(define (parse-def x)
  (kernel-syntax-case x #f
    [(define-values (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-syntax-def x)
  (kernel-syntax-case x #f
    [(define-syntaxes (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))


(define (add-type-name! names)
  (for-each register-type-name names))

(define (parse-type-alias form)
  (kernel-syntax-case* form #f 
    (define-type-alias-internal void)
    [(#%plain-app void (quote-syntax (define-type-alias-internal nm ty))) 
     (values #'nm #'ty)]
    [_ (int-err "not define-type-alias")]))

(define (type-check forms0)
  (begin-with-definitions
    (define forms (syntax->list forms0))
    (define-values (type-aliases struct-defs stx-defs0 val-defs0 provs reqs)
      (filter-multiple 
       forms
       (internal-syntax-pred define-type-alias-internal)
       (lambda (e) (or ((internal-syntax-pred define-typed-struct-internal) e)
                       ((internal-syntax-pred define-typed-struct/exec-internal) e)))
       parse-syntax-def
       parse-def 
       provide?
       define/fixup-contract?))
    (for-each (compose register-type-alias parse-type-alias) type-aliases)   
    ;; add the struct names to the type table
    (for-each (compose add-type-name! names-of-struct) struct-defs)
    ;; resolve all the type aliases, and error if there are cycles
    (resolve-type-aliases parse-type)
    ;; do pass 1, and collect the defintions
    (define defs (filter list? (map tc-toplevel/pass1 forms)))
    ;; separate the definitions into structures we'll handle for provides    
    (define stx-defs (filter def-stx-binding? defs))
    (define val-defs (filter def-binding? defs))
    ;; check that declarations correspond to definitions
    (check-all-registered-types)
    ;; typecheck the expressions and the rhss of defintions
    (for-each tc-toplevel/pass2 forms)
    ;; compute the new provides
    (with-syntax
        ([((new-provs ...) ...) (map (generate-prov stx-defs val-defs) provs)]
         [(new-defs ...) (map generate-contract-def reqs)])
      #`((begin new-defs ...)
         (begin
           #,(env-init-code)
           #,(tname-env-init-code)
           #,(talias-env-init-code)
           (begin new-provs ... ...))))))

;; typecheck a top-level form
;; used only from #%top-interaction
;; syntax -> void
(define (tc-toplevel-form form)
  (tc-toplevel/pass1 form)  
  (tc-toplevel/pass2 form))



