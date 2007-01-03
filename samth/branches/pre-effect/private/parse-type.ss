(module parse-type mzscheme
  
  (provide parse-type parse-type/id)
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))
  
  (require "types.ss"
           "infer.ss"
           "types-aux.ss"
           "subtype.ss"
           "remove-intersect.ss"
           "planet-requires.ss"
           "tc-utils.ss"
           "type-environments.ss"
           "subst.ss"
           "type-name-env.ss"
           (all-except (lib "list.ss"  "srfi" "1") unfold remove))
  
  
  (define (parse-type/id loc stx)
    #;(printf "parse-type/id id : ~a~n ty: ~a~n" (syntax-object->datum loc) (syntax-object->datum stx))
    (let* ([datum (syntax-object->datum stx)]
           [stx* (datum->syntax-object loc datum stx stx)])
      (parse-type stx*)))
  
  
  (define (parse-type stx)
    (parameterize ([current-orig-stx stx])
      (syntax-case* stx (-> list-of U Un All case-lambda vector-of Listof Vectorof values pred cons quote) symbolic-identifier=?
        [id
         (identifier? #'id)
         (lookup-type-name #'id
                 (lambda ()
                   (lookup (current-tvars) (syntax-e #'id) 
                           (lambda (key) (tc-error "unbound type ~a" key)))))]
        [(fst . rst)
         (not (syntax->list #'rst))
         (make-pair-ty (parse-type #'fst) (parse-type #'rst))]
        [(cons fst rst) (make-pair-ty (parse-type #'fst) (parse-type #'rst))]
        [(pred t) (make-pred-ty (parse-type #'t))]
        [(dom ... rest ::: -> rng)
         (symbolic-identifier=? #'::: (quote-syntax ..))
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-type #'rest) (parse-type #'rng))]
        ;; has to be below the previous one
        [(dom ... -> rng) 
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-type #'rng))]
        [(values tys ...) (make-values-ty (map parse-type (syntax->list #'(tys ...))))]
        [(case-lambda tys ...) (make-funty (map (lambda (ty) 
                                                  (syntax-case* ty (->) symbolic-identifier=?
                                                    [(dom ... -> rng)
                                                     (make-arr 
                                                      (map parse-type (syntax->list #'(dom ...))) 
                                                      (parse-type #'rng))]))
                                                (syntax->list #'(tys ...))))]
        ;; I wish I could write this
        #;[(case-lambda ([dom ... -> rng] ...)) (make-funty (list (make-arr (list (parse-type #'dom) ...) (parse-type #'rng)) ...))]
        #;[(list-of t) (make-lst (parse-type #'t))]
	#;[(Listof t) (make-lst (parse-type #'t))]
	[(Vectorof t) (make-vec (parse-type #'t))]
        [(vector-of t) (make-vec (parse-type #'t))]
	[(U ts ...) (apply Un (map parse-type (syntax->list #'(ts ...))))]
        [(Un ts ...) (apply Un (map parse-type (syntax->list #'(ts ...))))]
        [(quote t)
         (make-value (syntax-e #'t))]
        [(All (vars ...) t) 
         (andmap identifier? (syntax->list #'(vars ...)))
         (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
                [tvars (map make-tvar vars)])
           (parameterize ([current-tvars (extend-env vars tvars (current-tvars))])
             (make-poly vars (parse-type #'t))))]
        [(id arg args ...)
         (identifier? #'id)
         (let ([ty (parse-type #'id)])
           (unless (poly? ty)
             (tc-error "not a polymorphic type: ~a" (syntax-e #'id)))
           (unless (= (length (syntax->list #'(arg args ...))) (length (poly-var ty)))
             (tc-error "wrong number of arguments to type constructor ~a" (syntax-e #'id)))
           (let loop ([tyargs (syntax->list #'(arg args ...))]
                      [tyvars (poly-var ty)]
                      [t (poly-type ty)])          
             (cond [(null? tyargs) t]
                   [else 
                    (loop (cdr tyargs) (cdr tyvars) (subst (car tyvars) (parse-type (car tyargs)) t))])))]
        [t
         (or (boolean? (syntax-e #'t)) (number? (syntax-e #'t))
             (string? (syntax-e #'t)))
         (make-value (syntax-e #'t))]
        [_ (tc-error "not a valid type: ~a" stx)])))
  
  )