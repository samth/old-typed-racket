#lang scheme/unit

;; tests needed
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
                                   define-typed-struct/exec-internal
                                   require/typed-internal : void)
      ;; forms that are handled in other ways
      [stx 
       (or (syntax-property form 'typechecker:ignore) (syntax-property form 'typechecker:ignore-some))
       (list)]
      
      ;; directives to the typechecker
      [(#%plain-app void (quote-syntax (require/typed-internal nm ty)))
       (begin
         (printf/log "Require/typed ~a~n" (syntax-e #'nm))
         (register-type #'nm (parse-type #'ty)))]
      [(#%plain-app void (quote-syntax (define-type-alias-internal (nm . vars) ty)))
       (begin
         (printf/log "Type Alias ~a~n" (syntax-e #'nm))
         (register-type-name #'nm (parse-type #'(All vars ty))))]
      [(#%plain-app void (quote-syntax (define-type-alias-internal nm ty)))
       (begin
         (printf/log "Type Alias ~a~n" (syntax-e #'nm))
         (register-type-name #'nm (parse-type #'ty)))]
      
      [(#%plain-app void (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...))))
       (begin
         (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
         (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...))))]
      [(#%plain-app void (quote-syntax (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty)))
       (begin
         (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
         (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #'proc-ty))]
      [(#%plain-app void (quote-syntax (define-typed-struct-internal (vars ...) nm ([fld : ty] ...))))
       (begin
         (printf/log "Typed Struct ~a~n" (syntax-e #'nm))
         (tc/poly-struct (syntax->list #'(vars ...)) #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...))))]
      
      [(#%plain-app void (quote-syntax (define-type-internal nm top-pred elems ...)))
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
         #;(printf "d-s form: ~a~n" form)
         (map make-def-stx-binding (syntax->list #'(var ...))))]
      ;; otherwise, do nothing in this pass
      [_ (begin #;(printf "unk form: ~a~n" (syntax->datum form)) (list))])))





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
       [(#%require . _) (void)]
       [(#%provide . _) (void)]
       [(define-syntaxes . _) (void)]
       [(define-values-for-syntax . _) (void)]
       
       ;; these forms are handled in pass1
       [(#%plain-app void (quote-syntax (require/typed-internal . rest))) (void)]
       [(#%plain-app void (quote-syntax (define-type-alias-internal . rest))) (void)]
       [(#%plain-app void (quote-syntax (define-typed-struct-internal . rest))) (void)]        
       [(#%plain-app void (quote-syntax (define-type-internal . rest))) (void)]        
       
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
