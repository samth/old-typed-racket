(module typechecker-unit (lib "a-unit.ss")
  
  ;; tests needed
  
  (require (rename (lib "1.ss" "srfi") assoc* assoc)
           (prefix 1: (lib "1.ss" "srfi"))
           (lib "kerncase.ss" "syntax")
           (lib "struct.ss" "syntax")
           (lib "stx.ss" "syntax")
           (lib "etc.ss")
           (all-except (lib "list.ss") remove)
           "type-contract.ss"
           "signatures.ss"
           "tc-structs.ss"
           "utils.ss" ;; doesn't need tests
           "type-rep.ss" ;; doesn't need tests
           "unify.ss" ;; needs tests
           "infer.ss"
           "type-effect-convenience.ss" ;; maybe needs tests
           "union.ss"
           "subtype.ss" ;; has tests
           "internal-forms.ss" ;; doesn't need tests
           "remove-intersect.ss" ;; has tests
           "planet-requires.ss" ;; doesn't need tests
           "type-env.ss" ;; maybe needs tests
           "parse-type.ss" ;; has tests
           "tc-utils.ss" ;; doesn't need tests
           "type-environments.ss" ;; doesn't need tests
           "lexical-env.ss" ;; maybe needs tests
           "type-annotation.ss" ;; has tests
           "type-name-env.ss" ;; maybe needs tests
           "init-envs.ss"
           "effect-rep.ss"
           "mutated-vars.ss"
           (lib "plt-match.ss"))
  
  (require-for-template (lib "match.ss") "internal-forms.ss" "tc-utils.ss" (lib "contract.ss") mzscheme)
  (require-for-syntax (lib "match.ss") "internal-forms.ss")
  
  (require-galore)
  
  
  (import tc-if^ tc-lambda^ tc-app^ tc-let^)
  (export typechecker^)
  
  ;; check that expr has type ty in context of stx
  ;; syntax syntax type -> void
  (define (check-expr stx expr ty)
    (check-type stx (tc-expr/t expr) ty))
  
  ;; return the type of a literal value
  ;; scheme-value -> type
  (define (tc-literal v-stx)
    ;; find the meet of the types of a list of expressions
    ;; list[syntax] -> type
    (define (types-of-literals es)
      (apply Un (map tc-literal es)))
    (define v (syntax-e v-stx))
    (cond
      [(number? v) N]
      [(char? v) -Char]
      [(boolean? v) (-val v)]
      [(null? v) (-val null)]
      [(symbol? v) (-val v)]
      [(string? v) -String]
      [(keyword? v) -Keyword]
      [(bytes? v) -Bytes]
      [(list? v) (make-Listof (types-of-literals v))]
      [(vector? v) (make-Vector (types-of-literals (vector->list v)))]
      [(pregexp? v) -PRegexp]
      [(byte-pregexp? v) -Byte-PRegexp]
      [(byte-regexp? v) -Byte-Regexp]
      [(regexp? v) -Regexp]
      [else (begin (printf "checking literal : ~a~n" v) Univ)]))
  
  ;; typecheck an identifier
  ;; the identifier has variable effect
  ;; tc-id : identifier -> tc-result
  (define (tc-id id)
    (let* ([ty (lookup-type/lexical id)]
           [inst (syntax-property id 'type-inst)])
      (when (and inst
                 (not (Poly? ty)))
        (tc-error "Cannot instantiate non-polymorphic type ~a" ty))
      (when (and inst
                 (not (= (length (syntax->list inst)) (Poly-n ty))))
        (tc-error "Wrong number of type arguments to polymorphic type ~a:~nexpected: ~a~ngot: ~a"
                  ty (Poly-n ty) (length (syntax->list inst))))
      (let ([ty* (if inst
                     (begin
                       (printf/log "Type Instantiation: ~a~n" (syntax-e id))
                       (instantiate-poly ty (map parse-type (syntax->list inst))))
                     ty)])
        (ret ty* (list (make-Var-True-Effect id)) (list (make-Var-False-Effect id))))))
  
  ;; typecheck an expression, but throw away the effect
  ;; tc-expr/t : Expr -> Type
  (define (tc-expr/t e) (match (tc-expr e)
                          [(tc-result: t) t]))
  
  (define (check-below tr1 expected)
    (match (list tr1 expected)      
      [(list (tc-result: t1 te1 ee1) t2)
       (unless (subtype t1 t2)
         (tc-error "Expected ~a, but got ~a" t2 t1))
       (ret expected)]
      [(list t1 t2)
       (unless (subtype t1 t2)
         (tc-error "Expected ~a, but got ~a" t2 t1))
       (ret expected)]
      [_ (error "bad arguments to check-below")]))
  
  (define (tc-expr/check form expected)
    (parameterize ([current-orig-stx form])
      ;(printf "form: ~a~n" (syntax-object->datum form))
      ;; the argument must be syntax
      (unless (syntax? form) 
        (int-err "bad form input to tc-expr: ~a" form))
      (let (;; a local version of ret that does the checking
            [ret 
             (lambda args
               (define te (apply ret args))
               (check-below te expected)
               te)])
        (kernel-syntax-case* form #f 
          (letrec-syntaxes+values) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
          [stx
           (syntax-property form 'typechecker:with-handlers)
           (check-subforms/with-handlers/check form expected)]
          [stx 
           (syntax-property form 'typechecker:ignore-some)
           (let ([ty (check-subforms/ignore form)])
             (unless ty
               (tc-error "internal error: ignore-some"))
             (check-below ty expected))]
          ;; data
          [(#%datum . #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
          [(#%datum . #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
          [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
          [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
          
          [(#%datum . val) (ret (tc-literal #'val))]
          [(quote val)  (ret (tc-literal #'val))]
          ;; syntax
          [(quote-syntax datum) (ret -Syntax)]
          ;; mutation!
          [(set! id val)
           (match-let* ([(tc-result: id-t) (tc-id #'id)]
                        [(tc-result: val-t) (tc-expr #'val)])
             (unless (subtype val-t id-t)
               (tc-error "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
             (ret -Void))]
          ;; top-level variable reference - occurs at top level
          [(#%top . id) (check-below (tc-id #'id) expected)]
          ;; weird
          [(#%variable-reference . _)
           (tc-error "do not use #%variable-reference")]
          ;; identifiers
          [x (identifier? #'x) (check-below (tc-id #'x) expected)]
          ;; w-c-m
          [(with-continuation-mark e1 e2 e3)
           (begin (tc-expr/check #'e1 Univ)
                  (tc-expr/check #'e2 Univ)
                  (tc-expr/check #'e3 expected))]  
          ;; application        
          [(#%app . _) (tc/app/check form expected)]
          ;; syntax
          ;; for now, we ignore the rhs of macros
          [(letrec-syntaxes+values stxs vals . body)
           (tc-expr/check (syntax/loc form (letrec-values vals . body)) expected)]
          ;; begin
          [(begin e . es) (tc-exprs/check (syntax->list #'(e . es)) expected)]
          [(begin0 e . es)
           (begin (tc-exprs/check (syntax->list #'es) Univ)
                  (tc-expr/check #'e expected))]          
          ;; if
          [(if tst body) (tc/if-onearm/check #'tst #'body expected)]
          [(if tst thn els) (tc/if-twoarm/check #'tst #'thn #'els expected)]
          ;; lambda
          [(lambda formals . body)
           (tc/lambda/check form #'(formals) #'(body) expected)]        
          [(case-lambda [formals . body] ...)
           (tc/lambda/check form #'(formals ...) #'(body ...) expected)]      
          ;; let
          [(let-values ([(name ...) expr] ...) . body)
           (tc/let-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
          [(letrec-values ([(name ...) expr] ...) . body)
           (tc/letrec-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
          ;; other
          [_ (tc-error "cannot typecheck unknown form : ~a~n" (syntax-object->datum form))]
          ))))
  
  ;; type check form in the current type environment
  ;; if there is a type error in form, or if it has the wrong annotation, error
  ;; otherwise, produce the type of form
  ;; syntax[expr] -> type
  (define (tc-expr form)
    ;; do the actual typechecking of form
    ;; internal-tc-expr : syntax -> Type
    (define (internal-tc-expr form)
      (kernel-syntax-case* form #f 
        (letrec-syntaxes+values) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
        ;; 
        [stx
         (syntax-property form 'typechecker:with-handlers)
         (let ([ty (check-subforms/with-handlers form)])
           (unless ty
             (tc-error "internal error: with-handlers"))
           ty)]
        [stx 
         (syntax-property form 'typechecker:ignore-some)
         (let ([ty (check-subforms/ignore form)])
           (unless ty
             (tc-error "internal error: ignore-some"))
           ty)]
        
        ;; data        
        [(#%datum . #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
        [(#%datum . #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
        [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
        [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
        
        [(#%datum . val) (ret (tc-literal #'val))]
        [(quote val)  (ret (tc-literal #'val))]
        ;; syntax
        [(quote-syntax datum) (ret -Syntax)]
        ;; w-c-m
        [(with-continuation-mark e1 e2 e3)
         (begin (tc-expr/check #'e1 Univ)
                (tc-expr/check #'e2 Univ)
                (tc-expr #'e3))]
        ;; lambda
        [(lambda formals . body)
         (tc/lambda form #'(formals) #'(body))]        
        [(case-lambda [formals . body] ...)
         (tc/lambda form #'(formals ...) #'(body ...))]      
        ;; let
        [(let-values ([(name ...) expr] ...) . body)
         (tc/let-values #'((name ...) ...) #'(expr ...) #'body form)]
        [(letrec-values ([(name ...) expr] ...) . body)
         (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body form)]        
        ;; mutation!
        [(set! id val)
         (match-let* ([(tc-result: id-t) (tc-id #'id)]
                      [(tc-result: val-t) (tc-expr #'val)])
           (unless (subtype val-t id-t)
             (tc-error "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
           (ret -Void))]        
        ;; top-level variable reference - occurs at top level
        [(#%top . id) (tc-id #'id)]
        ;; weird
        [(#%variable-reference . _)
         (tc-error "do not use #%variable-reference")]
        ;; identifiers
        [x (identifier? #'x) (tc-id #'x)]                 
        ;; application        
        [(#%app . _) (tc/app form)]
        ;; if
        [(if tst body) (tc/if-twoarm #'tst #'body #'(#%app void))]
        [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els)]                          
        
        ;; syntax
        ;; for now, we ignore the rhs of macros
        [(letrec-syntaxes+values stxs vals . body)
         (tc-expr (syntax/loc form (letrec-values vals . body)))]
        
        ;; begin
        [(begin e . es) (tc-exprs (syntax->list #'(e . es)))]
        [(begin0 e . es)
         (begin (tc-exprs (syntax->list #'es))
                (tc-expr #'e))]
        ;; other
        [_ (tc-error "cannot typecheck unknown form : ~a~n" (syntax-object->datum form))]))
    
    (parameterize ([current-orig-stx form])
      ;(printf "form: ~a~n" (syntax-object->datum form))
      ;; the argument must be syntax
      (unless (syntax? form) 
        (int-err "bad form input to tc-expr: ~a" form))
      ;; typecheck form
      (cond [(type-annotation form) => (lambda (ann) (tc-expr/check form ann))]
            [else (internal-tc-expr form)])))
  
  ;; type-check a list of exprs, producing the type of the last one.
  ;; if the list is empty, the type is Void.
  ;; list[syntax[expr]] -> tc-result
  (define (tc-exprs exprs)
    (cond [(null? exprs) (ret -Void)]
          [(null? (cdr exprs)) (tc-expr (car exprs))]
          [else (tc-expr/check (car exprs) Univ)
                (tc-exprs (cdr exprs))]))
  
  (define (tc-exprs/check exprs expected)
    (cond [(null? exprs) (check-below (ret -Void) expected)]
          [(null? (cdr exprs)) (tc-expr/check (car exprs) expected)]
          [else (tc-expr (car exprs) Univ)
                (tc-exprs/check (cdr exprs) expected)]))
  
  (define-struct binding (name) #f)
  (define-struct (def-binding binding) (ty) #f)
  (define-struct (def-stx-binding binding) () #f)
  
  (define (tc-toplevel/pass1 form)
    #;(printf "form-top: ~a~n" form)
    ;; first, find the mutated variables:
    (find-mutated-vars form)
    (parameterize ([current-orig-stx form])
      (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                     define-typed-struct/exec-internal
                                     require/typed-internal : void)
        ;; forms that are handled in other ways
        [stx 
         (or (syntax-property form 'typechecker:ignore) (syntax-property form 'typechecker:ignore-some))
         (list)]
        
        ;; directives to the typechecker
        [(#%app void (quote-syntax (require/typed-internal nm ty)))
         (begin
           (printf/log "Require/typed ~a~n" (syntax-e #'nm))
           (register-type #'nm (parse-type #'ty)))]
        [(#%app void (quote-syntax (define-type-alias-internal (nm . vars) ty)))
         (begin
           (printf/log "Type Alias ~a~n" (syntax-e #'nm))
           (register-type-name #'nm (parse-type #'(All vars ty))))]
        [(#%app void (quote-syntax (define-type-alias-internal nm ty)))
         (begin
           (printf/log "Type Alias ~a~n" (syntax-e #'nm))
           (register-type-name #'nm (parse-type #'ty)))]
        
        [(#%app void (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...))))
         (begin
           (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
           (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...))))]
        [(#%app void (quote-syntax (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty)))
         (begin
           (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
           (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #'proc-ty))]
        [(#%app void (quote-syntax (define-typed-struct-internal (vars ...) nm ([fld : ty] ...))))
         (begin
           (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
           (tc/poly-struct (syntax->list #'(vars ...)) #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...))))]
        
        [(#%app void (quote-syntax (define-type-internal nm top-pred elems ...)))
         (let ([variants0 (map (lambda (stx)
                                 (syntax-case stx ()
                                   [(variant maker (fld ty) ...)
                                    (list #'variant #'maker (map cons (syntax->list #'(ty ...)) (syntax->list #'(fld ...))))]))
                               (syntax->list #'(elems ...)))])
           (tc/define-type #'nm #'top-pred variants0))]
        
        [(define-values (var ...) expr)
         (let* ([vars (syntax->list #'(var ...))])
           (cond
             [(andmap (lambda (s) (syntax-property s 'type-label)) (syntax->list #'(var ...)))            
              (let ([ts (map get-type vars)])
                (for-each register-type vars ts)
                (for-each (lambda (v) (printf/log "Top Level Define: ~a~n" (syntax-e v))) vars)
                (syntax-case #'expr (lambda)
                  [(lambda (vars ...) . _)
                   (for-each (lambda (v) (printf/log "Top Level Lambda: ~a~n" (syntax-e v))) (syntax->list #'(vars ...)))]
                  [_ (void)])
                (map make-def-binding vars ts))]
             [(and (= 1 (length vars)) 
                   (with-handlers ([exn:fail? (lambda _ #f)]) (tc-expr #'expr)))
              => (match-lambda 
                   [(tc-result: t)
                    (register-type (car vars) t)
                    (printf/log "Top Level Define: ~a~n" (syntax-e (car vars)))
                    (list (make-def-binding (car vars) t))])]
             [else
              (tc-error "Untyped definition")]))]
        ;; to handle the top-level, we have to recur into begins
        [(begin . rest)
         (apply append (filter list? (map tc-toplevel/pass1 (syntax->list #'rest))))]
        ;; define-syntaxes just get noted
        [(define-syntaxes (var ...) . rest)
         (andmap identifier? (syntax->list #'(var ...)))
         (begin
           #;(printf "form: ~a~n" form)
           (map make-def-stx-binding (syntax->list #'(var ...))))]
        ;; otherwise, do nothing in this pass
        [_ (list)])))
  
  
  
  ;; find the subexpressions that need to be typechecked in an ignored form
  ;; syntax -> void
  (define (check-subforms/with-handlers form)
    (define handler-tys '())
    (define body-ty #f)    
    (define (get-result-ty t)
      (match t
        [(Function: (list (arr: _ rngs _ _ _) ...)) (apply Un rngs)]
        [_ (tc-error "Internal error in get-result-ty: not a function type: ~n~a" t)]))
    (let loop ([form form])
      (parameterize ([current-orig-stx form])
        (kernel-syntax-case* form #f ()
          [stx
           ;; if this needs to be checked
           (syntax-property form 'typechecker:with-type)
           ;; the form should be already ascribed the relevant type
           (void 
            (tc-expr form))]
          [stx
           ;; this is a hander function
           (syntax-property form 'typechecker:exn-handler)
           (let ([t (tc-expr/t form)])
             (unless (subtype t (-> (Un) Univ))
               (tc-error "Exception handler must be a single-argument function, got ~n~a"))
             (set! handler-tys (cons (get-result-ty t) handler-tys)))]
          [stx
           ;; this is the body of the with-handlers
           (syntax-property form 'typechecker:exn-body)
           (let ([t (tc-expr/t form)])
             (set! body-ty t))]
          [(a . b)
           (begin
             (loop #'a)
             (loop #'b))]
          [_ (void)])))
    (ret (apply Un body-ty handler-tys)))
  
  (define (check-subforms/with-handlers/check form expected)
    (let loop ([form form])
      (parameterize ([current-orig-stx form])
        (kernel-syntax-case* form #f ()
          [stx
           ;; if this needs to be checked
           (syntax-property form 'typechecker:with-type)
           ;; the form should be already ascribed the relevant type
           (tc-expr form)]
          [stx
           ;; this is a hander function
           (syntax-property form 'typechecker:exn-handler)
           (tc-expr/check form (-> (Un) expected))]
          [stx
           ;; this is the body of the with-handlers
           (syntax-property form 'typechecker:exn-body)
           (tc-expr/check form expected)]
          [(a . b)
           (begin
             (loop #'a)
             (loop #'b))]
          [_ (void)])))
    expected)
  
  ;; typecheck the expansion of a with-handlers form
  ;; syntax -> type
  (define (check-subforms/ignore form)
    (let loop ([form form])
      (kernel-syntax-case* form #f ()
        [stx
         ;; if this needs to be checked
         (syntax-property form 'typechecker:with-type)
         ;; the form should be already ascribed the relevant type
         (tc-expr form)]
        [(a . b)
         (loop #'a)
         (loop #'b)]
        [_ (void)])))
    
    
  ;; typecheck the expressions of a module-top-level form
  ;; no side-effects
  ;; syntax -> void
  (define (tc-toplevel/pass2 form)
    (reverse-begin
     (do-time 
      (if (eq? 'define-values (syntax-e (stx-car form)))
          (format "In Pass 2 : After ~a ~a"
                  'define-values 
                  (syntax-object->datum (stx-car (stx-cdr form))))
          (format "In Pass 2 : After ~a" (syntax-object->datum (stx-car form)))))
     (parameterize ([current-orig-stx form])
       (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                      require/typed-internal void)
         ;; these forms we have been instructed to ignore
         [stx 
          (syntax-property form 'typechecker:ignore)
          (void)]
         
         [stx 
          (syntax-property form 'typechecker:ignore-some)
          (check-subforms/ignore form)]
         
         ;; these forms should always be ignored
         [(require . _) (void)]
         [(require-for-syntax . _) (void)]
         [(require-for-template . _) (void)]
         [(provide . _) (void)]
         [(define-syntaxes . _) (void)]
         
         ;; these forms are handled in pass1
         [(#%app void (quote-syntax (require/typed-internal . rest))) (void)]
         [(#%app void (quote-syntax (define-type-alias-internal . rest))) (void)]
         [(#%app void (quote-syntax (define-typed-struct-internal . rest))) (void)]        
         [(#%app void (quote-syntax (define-type-internal . rest))) (void)]        
         
         ;; definitions just need to typecheck their bodies
         [(define-values (var ...) expr)
          (let* ([vars (syntax->list #'(var ...))]
                 [ts (map lookup-type vars)])
            (tc-expr/check #'expr (list->values-ty ts)))]
        ;; to handle the top-level, we have to recur into begins
         [(begin) (void)]
         [(begin . rest)
          (let loop ([l (syntax->list #'rest)])
            (if (null? (cdr l))
                (tc-toplevel/pass2 (car l))
                (begin (tc-toplevel/pass2 (car l))
                       (loop (cdr l)))))]
         
         ;; otherwise, the form was just an expression
         [_ (tc-expr form)]))))
  
  ;; typecheck a top-level form
  ;; used only from #%top-interaction
  ;; syntax -> void
  (define (tc-toplevel-form form)
    (tc-toplevel/pass1 form)
    (tc-toplevel/pass2 form))
  
  
  (define (provide? form)
    (kernel-syntax-case form #f
      [(provide . rest) form]
      [_ #f]))
  
  (define ((generate-prov stx-defs val-defs) form)
    (define (mem? i vd)
      (cond [(1:member i vd (lambda (i j) (module-identifier=? i (binding-name j)))) => car]
            [else #f]))
    (define (lookup-id i vd)
      (def-binding-ty (mem? i vd)))
    (define (mk internal-id external-id)
      (cond
        [(mem? internal-id val-defs) 
         =>
         (lambda (b)
           (with-syntax ([id internal-id]
                         [out-id external-id])
             (cond [(type->contract (def-binding-ty b) (lambda () #f)) 
                    =>
                    (lambda (cnt)                                    
                      (with-syntax ([(export-id cnt-id) (generate-temporaries #'(id id))])
                        #`(begin 
                            (define/contract cnt-id #,cnt id)
                            (define-syntax export-id
                              (if (unbox typed-context?)
                                  (make-rename-transformer #'id)
                                  (make-rename-transformer #'cnt-id)))
                            (provide (rename export-id out-id)))))]
                   [else 
                    (with-syntax ([(export-id) (generate-temporaries #'(id))])
                        #`(begin                             
                            (define-syntax export-id
                              (if (unbox typed-context?)
                                  (make-rename-transformer #'id)
                                  (lambda (stx) (tc-error/stx stx "The type of ~a cannot be converted to a contract" (syntax-e #'id)))))
                            (provide (rename export-id out-id))))])))]
        [(mem? internal-id stx-defs) 
         =>
         (lambda (b)
           (with-syntax ([id internal-id]
                         [out-id external-id])
             (with-syntax ([(export-id cnt-id) (generate-temporaries #'(id id))])
               #`(begin                    
                   (define-syntax export-id
                     (if (unbox typed-context?)
                         (make-rename-transformer #'id)
                         (lambda (stx)
                           (tc-error/stx stx "Macro ~a from typed module used in untyped code" (syntax-e #'out-id)))))
                   (provide (rename export-id out-id))))))]
        [(eq? (syntax-e internal-id) (syntax-e external-id))
         #`(provide #,internal-id)]
        [else #`(provide (rename #,internal-id #,external-id))]))
    (kernel-syntax-case form #f
      [(provide form ...)
       (map 
        (lambda (f)
          (parameterize ([current-orig-stx f])
            (syntax-case* f (struct rename all-defined protect all-defined-except all-from all-from-except) 
              (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
              [id 
               (identifier? #'id)
               (mk #'id #'id)]
              [(all-from . rest) #`(provide #,f)]
              [(all-from-except . rest) #`(provide #,f)]
              [(rename in out)
               (mk #'in #'out)]
              [(protect . _)
               (tc-error "provide: protect not supported by Typed Scheme")]
              [(struct . _)
               (tc-error "provide: struct not supported by Typed Scheme")]
              [(all-defined . _)
               (tc-error "provide: all-defined not supported by Typed Scheme")]
              [(all-defined-except . _)
               (tc-error "provide: all-defined-except not supported by Typed Scheme")])))
        (syntax->list #'(form ...)))]))
  
  ;; type check a list of module-level forms
  ;; produce code to add the definitions in this module to the global table of module-level forms
  ;; syntax-list -> syntax
  (define (type-check forms)
    (begin-with-definitions 
      (define forms* (syntax->list forms))
      (do-time "Before Pass1")
      ;; install defined names and types in the environment
      ;; figure out all the bindings
      #;(printf "forms: ~a~n" forms*)
      (define defs (apply append (filter list? 
                                         (map tc-toplevel/pass1 forms*))))
      (define stx-defs (filter def-stx-binding? defs))
      (define val-defs (filter def-binding? defs))
      (do-time "Before Pass2")
      ;; typecheck the expressions
      (for-each tc-toplevel/pass2 forms*)
      (do-time "After Pass2") 
      (let* ([provs (filter (lambda (x) x) (map provide? forms*))])
        (with-syntax
            ([((new-provs ...) ...) (map (generate-prov stx-defs val-defs) provs)])
          (begin0
            #`(begin
                #,(env-init-code)
                #,(tname-env-init-code)
                (begin new-provs ... ...))
            (do-time "After Code Gen"))))))
  
  ;(trace subtype-of)
  ;(trace check-expr)
  ;(trace tc-expr)
  ;(trace intersect-ty)
  ;(trace remove-ty)
  ;(trace all-in)
  ;(trace symbolic-identifier=?)
  
  ;(trace parse-type)
  
  
  )