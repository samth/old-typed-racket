(module signatures mzscheme
  (require (lib "unit.ss"))
  (provide (all-defined))
  
  (define-signature type-printer^
    (print-type has-name))
  
  (define-signature infer^
    (unify unify1 fv fv/list unfold))
  
  (define-signature subst^
    (subst subst-all))
  
  (define-signature type-equal^
    (type-equal? type-compare type<? rename))
  
  )