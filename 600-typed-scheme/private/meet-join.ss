;(require (lib "debug.ss""macro-debugger" "model"))
;(trace-verbose? #t)
; (trace #'
(module meet-join mzscheme
  
  (require "union.ss" "type-rep.ss"
           "subtype.ss"
           "rep-utils.ss"
           "type-effect-convenience.ss"
           "planet-requires.ss"
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (require-schemeunit)
  
  (provide meet join promote demote)
  (define (join a b) (Un a b))
  
  (define (meet a b)
    (cond [(type-equal? a b) a]
          [(subtype a b) a]
          [(subtype b a) b]
          [else (Un)]))
  
  (define (promote t V)
    (define (sb t) (promote t V))
    (if (zero? (hash-table-count (free-vars* t)))
        t
        (match t
          [(F: (? (lambda (v) (memq v V)))) Univ]
          [(Vector: _) Univ]
          [(Box: _) Univ]
          [(Pair: a b) (make-Pair (promote a V) (promote b V))]
          [_ t])))
  
  (define (demote t V)
    (if (zero? (hash-table-count (free-vars* t)))
        t
        (match t
          [(F: (? (lambda (v) (memq v V)))) (Un)]
          [(Vector: _) (Un)]
          [(Box: _) (Un)]
          [_ t])))
  
  (define-struct constraint (lbound var ubound))
  
  (define-struct constraint-set (Xs V constraints))
  
  ;; lookup-constraint : Constraint-Set Tvar -> Constraint
  (define (lookup-constraint C X)
    (cond [(memf (lambda (v) (eq? (constraint-var v) X)) (constraint-set-constraints C)) => car]
          [else (make-constraint (Un) X Univ)]))
  
  ;; substitution-gen : Constraint-Set Type -> Substitution
  (define (substitution-gen C R)
    (define fvs (free-vars* R))
    (define substitution
      (hash-table-map fvs (lambda (k v)
                            (match-let ([(struct constraint (S Xi T)) (lookup-constraint C k)])
                              (evcase v
                                [(Constant Covariant) S]
                                [(Contravariant) T]
                                [else (if (type-equal? S T) 
                                          S
                                          (error "substitution undefined" C R k v))])))))
    substitution)
  
  
  )
;)