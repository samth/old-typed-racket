(module subst-unit mzscheme
  (require "planet-requires.ss" (lib "trace.ss") (lib "unit.ss")
           "signatures.ss"
           "type-def-structs.ss")
  
  (require-libs)
  (provide (all-defined))
  
  (define-unit subst@
    (import type-structs^)
    (export subst^)
    ;; FIXME!
    ;; this is NOT capture avoiding.  replacement should not have free vars (I think)
    ;; produce ty[replacement/var]
    (define (subst var replacement ty)
      (let ([sb (lambda (e) (subst var replacement e))])
        (match ty
          [($ tvar v) (if (eq? v var) replacement ty)]
          [($ funty ((? arr? elems) ...)) (make-funty (map (match-lambda
                                                             [($ arr ins out rest) (make-arr (map sb ins)
                                                                                             (sb out)
                                                                                             rest)])
                                                           elems))]
          [($ vec t) (make-vec (sb t))]
          [($ pred-ty t) (make-pred-ty (sb t))]
          [($ poly vs t) (if (memq var vs) ty (make-poly vs (sb t)))]
          [($ mu v t) (if (eq? v var) ty (make-mu v (sb t)))]
          [($ struct-ty nm par elems)
           (make-struct-ty nm par (map sb elems))]
          [($ union elems) 
           (make-union (set:map sb elems))]
          [($ values-ty vs) (make-values-ty (map sb vs))]
          [($ pair-ty a d) (make-pair-ty (sb a) (sb d))]
          [(or ($ base-type _)
               ($ value _)
               ($ dynamic)
               ($ univ)) 
           ty]
          [_ (car ty)])))
    
    ;; substitute many variables
    ;; subst-all : substition Type -> Type
    (define (subst-all s t)
      (foldr (lambda (e acc) (subst (car e) (cadr e) acc)) t s))
    )
  
  
  
  #;(trace subst)
  )