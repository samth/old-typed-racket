(module infer-unit mzscheme
  
  
  (require "planet-requires.ss" (lib "unit.ss") "signatures.ss"
           "type-def-structs.ss")
  (require-libs)
  
  (define-unit infer@
    
    (import subst^ type-structs^ type-equal^)
    (export infer^)
    ;; finds the free variables of a type
    ;; type -> Set[type]
    
    (define (fv t)
      (define (fv-of-list ts)
        (foldl (lambda (e acc) (set:union (fv e) acc)) (set:make-eq) ts))
      (match t
        [($ tvar v) (set:make-equal v)]
        [($ arr ts t #f) (foldr set:union (fv t) (map fv ts))]
        [($ funty ts) (foldr set:union (set:make-equal) (map fv ts))]
        [($ vec t) (fv t)]
        [($ mu v t) (set:remove v (fv t))]
        [($ poly vars t) (set:difference (fv t) (set:list->equal vars))]
        [($ union elems) (fv-of-list (set:elements elems))]
        [($ values-ty elems) (fv-of-list elems)]
        [($ pair-ty a d) (set:union (fv a) (fv d))]
        [($ struct-ty name parent flds)
         (foldr set:union (set:make-equal) (map fv flds))]
        [(or ($ base-type _)
             ($ value _)
             ($ dynamic)
             ($ univ)) 
         (set:make-equal)]))
    
    (define (fv/list t) (set:elements (fv t)))
    
    ;; unfold a recursive type one step
    (define (unfold t)
      (match t
        [($ mu v b) (subst v t b)]
        [_ (error "unfold failed" (type? t) t)]))
    
    ;; unify : list-of( list(type type)) -> Maybe[substitution]
    (define (unify cl) (unify/acc cl '()))
    
    (define (unify1 t1 t2) (unify (list (list t1 t2))))
    
    (define (unify/acc constraint-list acc)
      (parameterize ([match-equality-test type-equal?])
        (match constraint-list
          ;; done!
          [() acc]
          ;; equal constraints can be discarded
          [((t t) rest ...) (unify/acc rest acc)]
          ;; unify a variable (skipping the occurs check for now)
          [((($ tvar v) t) rest ...) 
           (unify/acc (map (lambda (p) (map (lambda (e) (subst v t e)) p)) rest) 
                      (cons (list v t) acc))]
          [((t ($ tvar v)) rest ...) 
           (unify/acc (map (lambda (p) (map (lambda (e) (subst v t e)) p)) rest) 
                      (cons (list v t) acc))]
          ;; arrow types - just add a whole bunch of new constraints
          [((($ funty (($ arr ts t t-rest) ...)) ($ funty (($ arr ss s s-rest) ...))) rest ...)
           (let ()
             (define (compatible-rest t-rest s-rest)
               (andmap (lambda (x y) (or (and x y) (and (not x) (not y)))) ;; either both #f or both not #f
                       t-rest s-rest))
             (define (flatten/zip x y) (map list (apply append x) (apply append y)))
             (if (and (= (length ts) (length ss))
                      (compatible-rest t-rest s-rest))
                 (let ([ret-constraints (map list t s)]
                       ;; remove the #f's before adding to the constraints
                       [rest-constraints (map list (filter values t-rest) (filter values s-rest))]
                       ;; transform ((a b c) (d e)) ((1 2 3) (4 5)) into ((a 1) (b 2) (c 3) (d 4) (e 5))
                       [arg-constraints (flatten/zip ts ss)])
                   #;(printf "constraints ~a~n"(append ret-constraints rest-constraints arg-constraints))
                   (unify/acc (append arg-constraints rest-constraints ret-constraints rest) acc))
                 #f))]
          ;; aggregate types are simple
          [((($ vec t) ($ vec s)) rest ...) (unify/acc (cons (list t s) rest) acc)]
          [((($ pair-ty t1 t2) ($ pair-ty s1 s2)) rest ...) 
           (unify/acc (list* (list t1 s1) (list t2 s2) rest) acc)]
          ;; just pretend dynamic works with anything
          [((or (_ ($ dynamic)) (($ dynamic) _)) rest ...) (unify/acc rest acc)]
          ;; structs
          [((($ struct-ty nm p elems) ($ struct-ty nm p elems*)) rest ...)
           (unify/acc (append rest (map list elems elems*)) acc)]
          ;; union types - oh no!
          [((($ union e1) ($ union e2)) rest ...)
           (let ([l1 (set:elements e1)]
                 [l2 (set:elements e2)])
             (and (= (length l1) (length l2))
                  (unify/acc (append (map list l1 l2) rest) acc)))]
          [((or (($ union _) _) (_ ($ union _))) rest ...) 
           #;(printf "FIXME: union type ~n~a~n---------~n~a~n in unifier~n" 
                     (caar constraint-list)
                     (cadar constraint-list)) 
             #f]
          ;; mu types
          [(((? mu? s) (? mu? t)) rest ...) 
           (unify/acc (cons (rename s t) rest) acc)]
          [((t (? mu? s)) rest ...) (unify/acc (cons (list t (unfold s)) rest) acc)]
          [(((? mu? s) t) rest ...) (unify/acc (cons (list (unfold s) t) rest) acc)]
          #;[((or (($ mu _ _) _) (_ ($ mu _ _))) rest ...) 
             (printf "FIXME: mu types ~a in unifier~n" constraint-list)
             #f]
          ;; polymorphic types - don't do that
          [((or (($ poly a b) _) (_ ($ poly a b))) rest ...)
           (printf "FIXME: poly type (poly ~a ~a) in unifier~n" a b)
           #f]
          ;; nothing else can have type variables
          [else #f]
          )))
    ;(trace unify/acc)
    )
  
  (provide (all-defined))  
  
  )