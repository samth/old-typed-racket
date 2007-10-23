(module tc-app-unit (lib "a-unit.ss")
  
  (require "signatures.ss"
           (lib "plt-match.ss")
           (lib "list.ss")
           "type-rep.ss"
           "effect-rep.ss"
           "tc-utils.ss"
           "subtype.ss"
           "unify.ss"
           "infer.ss"
           "type-effect-convenience.ss"
           "type-effect-printer.ss"
           (lib "pretty.ss")
           (lib "trace.ss")
           (lib "kerncase.ss" "syntax"))
  
  (require-for-template (lib "plt-match.ss") "internal-forms.ss" mzscheme)
  (require-for-syntax (lib "plt-match.ss") "internal-forms.ss")

  
  (import typechecker^)
  (export tc-app^)
  
    
  ;; comparators that inform the type system
  (define (comparator? i)
    (or (module-identifier=? i #'eq?)
        (module-identifier=? i #'equal?)
        (module-identifier=? i #'eqv?)
        (module-identifier=? i #'=)
        (module-identifier=? i #'string=?)))
  
  ;; typecheck eq? applications
  ;; identifier identifier expression expression expression
  ;; identifier expr expr expr expr -> tc-result
  (define (tc/eq comparator v1 v2)
    (define (e? i) (module-identifier=? i comparator))
    (define (do id val)
      (define-syntax alt (syntax-rules () [(_ nm pred ...) 
                                           (and (e? #'nm) (or (pred val) ...))]))
      (if (or (alt symbol=? symbol?)
              (alt string=? string?)
              (alt = number?)
              (alt eq? boolean? keyword? symbol?)
              (alt eqv? boolean? keyword? symbol? number?)
              (alt equal? (lambda (x) #t)))
          (values (list (make-Restrict-Effect (-val val) id))
                  (list (make-Remove-Effect (-val val) id)))          
          (values (list) (list))))
    (match (list (tc-expr v1) (tc-expr v2))
      [(list (tc-result: id-t (list (Var-True-Effect: id1)) (list (Var-False-Effect: id2))) (tc-result: (Value: val)))
       (do id1 val)]
      [(list (tc-result: (Value: val)) (tc-result: id-t (list (Var-True-Effect: id1)) (list (Var-False-Effect: id2))))
       (do id1 val)]
      [_ (values (list) (list))]))

  
    ;; typecheck an application:
  ;; arg-types: the types of the actual parameters
  ;; arg-effs: the effects of the arguments
  ;; dom-types: the types of the function's fixed arguments
  ;; rest-type: the type of the functions's rest parameter, or #f
  ;; latent-eff: the latent effect of the function
  ;; arg-stxs: the syntax for each actual parameter, for error reporting
  ;; [Type] [Type] Maybe[Type] [Syntax] -> Effect  
  (define (tc-args arg-types arg-thn-effs arg-els-effs dom-types rest-type latent-thn-eff latent-els-eff arg-stxs)
    (define (var-true-effect-v e) (match e
                                    [(Var-True-Effect: v) v]))
    (define (var-false-effect-v e) (match e
                                    [(Var-False-Effect: v) v]))
    ;; special case for predicates:
    (if (and (not (null? latent-thn-eff))
             (not (null? latent-els-eff))
             (not rest-type)
             ;(printf "got to =~n")
             (= (length arg-types) (length dom-types) 1)             
             ;(printf "got to var preds~n")
             (= (length (car arg-thn-effs)) (length (car arg-els-effs)) 1)
             (Var-True-Effect? (caar arg-thn-effs)) ;; thn-effs is a list for each arg
             (Var-False-Effect? (caar arg-els-effs)) ;; same with els-effs
             #;(printf "got to mi= ~a ~a ~n~a ~a~n" 
                     (var-true-effect-v (caar arg-thn-effs)) (var-true-effect-v (caar arg-els-effs))
                     (syntax-e (var-true-effect-v (caar arg-thn-effs))) (syntax-e (var-false-effect-v (caar arg-els-effs))))
             (module-identifier=? (var-true-effect-v (caar arg-thn-effs))
                                  (var-false-effect-v (caar arg-els-effs)))
             (subtype (car arg-types) (car dom-types)))
        ;; then this was a predicate application, so we construct the appropriate type effect
        (values (map (add-var (var-true-effect-v (caar arg-thn-effs))) latent-thn-eff)
                (map (add-var (var-true-effect-v (caar arg-thn-effs))) latent-els-eff))
        ;; otherwise, we just ignore the effects.
        (let loop ([args arg-types] [doms dom-types] [stxs arg-stxs])
          (cond 
            [(and (null? args) (null? doms)) (values null null)] ;; here, we just return the empty effect
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
             (tc-error/stx (car stxs) "Wrong function argument type, expected ~a, got ~a" (car doms) (car args))]))))
  
  
  ;(trace tc-args)

  (define (tc/apply f args)
    (let* ([f-ty (tc-expr f)]
           [arg-tys0 (map tc-expr/t (syntax->list args))])
      ;; produces the first n-1 elements of the list, and the last element
      (define (split l)
        (let loop ([l l] [acc '()])
          (if (null? (cdr l))
              (values (reverse acc) (car l))
              (loop (cdr l) (cons (car l) acc)))))
      (let-values ([(arg-tys tail-ty) (split arg-tys0)])
        (define (printable dom rst)
          (list dom rst '..))
        (match f-ty
          [(tc-result: (Function: (list (arr: doms rngs rests thn-effs els-effs) ..1)))
           (let loop ([doms* doms] [rngs* rngs] [rests* rests])
             (cond [(null? doms*) 
                    (if (and (not (null? doms)) (null? (cdr doms)))
                        (tc-error
                         "bad arguments to apply - function expected ~a fixed arguments and (Listof ~a) rest argument, given ~a"
                         (car doms) (car rests) arg-tys0)
                        (tc-error "no function domain matched - domains were: ~a arguments were ~a" 
                                  (map printable doms rests)
                                  arg-tys0))]
                   [(and (subtypes arg-tys (car doms*)) (car rests*) (subtype tail-ty (make-Listof (car rests*))))
                    (ret (car rngs*))]
                   [else (loop (cdr doms*) (cdr rngs*) (cdr rests*))]))]                      
          [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests thn-effs els-effs) ..1))))
           (for-each (lambda (x) (unless (not (Poly? x))                                      
                                   (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                     arg-tys0)
           (let loop ([doms* doms] [rngs* rngs] [rests* rests])
             (cond [(null? doms*)
                    (if (= 1 (length doms))
                        (tc-error "polymorphic function domain did not match - domain was: ~a arguments were ~a" 
                                  (car doms) arg-tys0)
                        (tc-error "no polymorphic function domain matched - domains were: ~a arguments were ~a" doms arg-tys0))]
                   [(and (= (length (car doms*))
                            (length arg-tys))
                         (infer/list (append (car doms*) (list (make-Listof (car rests*)))) arg-tys0 vars))
                    => (lambda (substitution) 
                         (ret (subst-all substitution (car rngs*))))]
                   [else (loop (cdr doms*) (cdr rngs*) (cdr rests*))]))]
          [(tc-result: (Poly: vars (Function: '())))
           (tc-error "Function has no cases")]
          [else (tc-error "~a is not a function type" f-ty)]))))
  
  (define (tc/funapp f args)
    (match-let* ([ftype (tc-expr f)]
                 [(list (tc-result: argtypes arg-thn-effs arg-els-effs) ...) (map tc-expr (syntax->list args))])
      (let outer-loop ([ftype ftype] 
                       [argtypes argtypes]
                       [arg-thn-effs arg-thn-effs]
                       [arg-els-effs arg-els-effs]
                       [args args])
        (match ftype
          [(tc-result: (and sty (Struct: _ _ _ (? Type? proc-ty))) thn-eff els-eff)
           (outer-loop (ret proc-ty thn-eff els-eff)
                       (cons sty argtypes) 
                       (cons (list) arg-thn-effs)
                       (cons (list) arg-els-effs)
                       #`(#,(syntax/loc f dummy) #,@args))]
          [(tc-result: (? Mu? t) thn-eff els-eff)
           (outer-loop (ret (unfold t) thn-eff els-eff) argtypes arg-thn-effs arg-els-effs args)]
          [(tc-result: (Param: in out))
           (match argtypes
             [(list) (ret out)]
             [(list t)
              (if (subtype t in) 
                  (ret -Void)
                  (tc-error "Wrong argument to parameter - expected ~a and got ~a" in t))]
             [_ (tc-error "Wrong number of arguments to parameter - expected 0 or 1, got ~a" (length argtypes))])]
          [(tc-result: (Function: (list (arr: doms rngs rests latent-thn-effs latent-els-effs) ..1)) thn-eff els-eff)
           (if (= 1 (length doms))
               (let-values ([(thn-eff els-eff)
                             (tc-args argtypes arg-thn-effs arg-els-effs (car doms) (car rests) 
                                      (car latent-thn-effs) (car latent-els-effs) 
                                      (syntax->list args))])
                 (ret (car rngs) thn-eff els-eff)
                 #;(if (false-effect? eff)
                       (ret (-val #f) eff)
                       (ret (car rngs) eff)))
               (let loop ([doms* doms] [rngs rngs] [rests rests])
                 (cond [(null? doms*) 
                        (tc-error "no function domain matched - domains were: ~a arguments were ~a" doms argtypes)]
                       [(subtypes/varargs argtypes (car doms*) (car rests)) (ret (car rngs))]
                       [else (loop (cdr doms*) (cdr rngs) (cdr rests))])))]
          [(tc-result: (Poly: vars (Function: (list (arr: doms rngs #f thn-effs els-effs) ...))))
           #;(printf "Typechecking poly app: ~a~n~n~a~n ~a~n" ftype argtypes doms)
           (for-each (lambda (x) (unless (not (Poly? x))
                                   (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                     argtypes)
           (match-let ([(tc-result: (Poly-names: msg-vars (Function: (list (arr: msg-doms msg-rngs #f _ _) ...)))) ftype])
             (let loop ([doms* doms] [rngs* rngs])
               (cond [(null? doms*)
                      (if (= 1 (length doms))
                          (tc-error "Polymorphic function could not be applied to arguments:~nExpected: ~a ~nActual: ~a" 
                                    (car msg-doms) argtypes)
                          (tc-error "no polymorphic function domain matched - domains were: ~a arguments were ~a" msg-doms argtypes))]
                     [(and (= (length (car doms*))
                              (length argtypes))
                           (infer/list (car doms*) argtypes vars))
                      => (lambda (substitution) 
                           #;(printf "subst is:~a~nret is: ~a~nvars is: ~a~n" substitution (car rngs*) vars)
                           (ret (subst-all substitution (car rngs*))))]
                     [else (loop (cdr doms*) (cdr rngs*))])))]
          ;; polymorphic varargs
          [(tc-result: (Poly: vars (Function: (list (arr: dom rng rest thn-eff els-eff)))))
           (for-each (lambda (x) (unless (not (Poly? x))                                      
                                   (tc-error "Polymorphic argument ~a to polymorphic function not allowed" x)))
                     argtypes)
           (unless (<= (length dom) (length argtypes))
             (tc-error "incorrect number of arguments to function: ~a ~a" dom argtypes))
           (let ([substitution (infer/list/vararg dom rest argtypes vars)])
             (if substitution
                 (ret (subst-all substitution rng))
                 (tc-error "no polymorphic function domain matched - domain was: ~a rest type was: ~a arguments were ~a"
                           dom rest argtypes)))]
          [(tc-result: (Poly: vars (Function: (list (arr: doms rngs rests thn-effs els-effs) ...))))
           (tc-error "polymorphic vararg case-lambda application not yet supported")]
          [else (tc-error "~a is not a function type" (tc-result-t ftype))]))))
  
  ;(trace tc/funapp)
  
  (define (tc/app form)
    (kernel-syntax-case* form #f 
      (values apply not list list* call-with-values) ;; the special-cased functions     
      ;; call-with-values
      [(#%app call-with-values prod con)
       (match-let* ([(tc-result: prod-t) (tc-expr #'prod)]
                    [(tc-result: con-t) (tc-expr #'con)])
         (match (list prod-t con-t)
           [(list (Function: (list (arr: (list) vals #f _ _))) (Function: (list (arr: dom rng #f _ _))))
            (=> unmatch)
            (match (list vals dom)
              [(list (Values: v) (list t ...))
               (if (subtypes v t)
                   (ret rng)
                   (unmatch))]
              [(list t1 (list t2))
               (if (subtype t1 t2) (ret rng) (unmatch))]
              [_ (unmatch)])]
           [_ (tc-error "Incorrect arguments to call with values: ~a ~a" prod-t con-t)]))]
      ;; special cases for `values'
      [(#%app values arg) (tc-expr #'arg)]
      [(#%app values . args)
       (let ([tys (map tc-expr/t (syntax->list #'args))])
         (ret (list->values-ty tys)))]
      ;; special case for `list'
      [(#%app list . args)
       (let ([tys (map tc-expr/t (syntax->list #'args))])
         (ret (apply -lst* tys)))]
      ;; special case for `list*'
      [(#%app list* . args)
       (match-let* ([(list last tys-r ...) (reverse (map tc-expr/t (syntax->list #'args)))]
                    [tys (reverse tys-r)])
         (ret (foldr make-Pair last tys)))]
      ;; in eq? cases, call tc/eq
      [(#%app eq? v1 v2) 
       (and (identifier? #'eq?) (comparator? #'eq?))
       (begin
         ;; make sure the whole expression is type correct
         (tc/funapp #'eq? #'(v1 v2))
         ;; check thn and els with the eq? info
         (let-values ([(thn-eff els-eff) (tc/eq #'eq? #'v1 #'v2)])
           (ret B thn-eff els-eff)))]
      ;; special case for `not'
      [(#%app not arg)
       (match (tc-expr #'arg)
         ;; if arg was a predicate application, we swap the effects
         [(tc-result: t thn-eff els-eff)
          (ret B (map var->type-eff els-eff) (map var->type-eff thn-eff))])]
      ;; special case for `apply'
      [(#%app apply f . args) (tc/apply #'f #'args)]         
      [(#%app f args ...) (tc/funapp #'f #'(args ...))]))
  
  )