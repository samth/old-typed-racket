(module remove-intersect mzscheme
  
  (require "types.ss" "types-aux.ss" "infer.ss" "subtype.ss" "planet-requires.ss" "subst.ss")
  
  (require-libs)
  
  ;; this is *definitely* not yet correct
  
  ;; NEW IMPL
  ;; restrict t1 to be a subtype of t2
  (define (restrict t1 t2)
    (define (unify/poly a b)
      (if (poly? b)
          (let* ([vs (poly-var b)]
                 [vs* (map gensym vs)]
                 [body* (subst-all (map (lambda (v v*) (list v (make-tvar v*))) vs vs*) (poly-type b))]
                 [subst (unify (list (list a body*)))])
            (if subst a #f))
          #f))
    (define (union-map f l)
      (apply Un (map f (set:elements (union-elems l)))))
    (cond [(subtype t1 t2) t1] ;; already a subtype
          [(unify/poly t1 t2)]
          [(union? t1) (union-map (lambda (e) (restrict e t2)) t1)]
          [(mu? t1) 
           (let* ([mv (mu-var t1)]
                  [mv* (gensym mv)]
                  [m-var (make-tvar mv*)]
                  [body* (subst mv m-var (mu-type t1))])
             (make-mu mv* (restrict body* t2)))]
          [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
          [else (Un)] ;; t2 and t1 have no intersection, so the result is the empty type
          ))

  ;; this is really restrict: restrict t1 to be a subtype of t2
  (define (intersect t1 t2)
    (cond [(subtype t1 t2) t1]
          [(subtype t2 t1) t2]
          [else (match (list t1 t2)
                  [(($ mu v b) t) (intersect b t)]
                  [(($ union l1) ($ union l2))
                   (make-union* (set:filter (lambda (e) (set:member? e l2)) l1))]
                  [(($ union l) t)
                   (make-union* (set:filter (lambda (e) (subtype e t)) l))]
                  [_ t1])]
        ))
  
  ;; also not yet correct
  ;; produces old without the contents of rem
  (define (remove old rem)
    (if (subtype old rem) 
        (Un) ;; the empty type
        (match (list old rem)
          [(($ union l) rem)
           (apply Un (map (lambda (e) (remove e rem)) (set:elements l)))]
          #;[(($ union l) t)
           (make-union* (set:filter (lambda (e) (not (type-equal? e t))) l))]
          #;[(t ($ union l2))
           (set:fold remove t l2)]      
          [(($ mu v b) t) (make-mu v (remove b rem))]
          [(($ poly v b) t) (make-poly v (remove b rem))]
          [_ old])))
    
  ;(trace remove)
  ;(trace intersect)
  ;(trace restrict)
  
  (provide (all-defined))

  )