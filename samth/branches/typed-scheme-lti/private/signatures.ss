(module signatures mzscheme
  (require (lib "unit.ss"))
  (provide (all-defined))
  
  ;; cycle 1
  
  (define-signature type-printer^
    (print-type has-name print-effect)) ;; done
  
  (define-signature infer^
    (unify1 fv fv/list unfold)) ;; done 
   
  (define-signature subst^
    (subst subst-all)) ;; done
  
  (define-signature type-equal^
    (type-equal? type-compare type<? rename tc-result-equal?)) ;; done
  
  ;; cycle 2
  
  (define-signature typechecker^
    (tc-expr tc-expr/check check-below tc-literal type-check tc-exprs tc-exprs/check tc-expr/t check-expr tc-toplevel-form))
  
  (define-signature tc-if^
    (tc/if-onearm tc/if-twoarm tc/if-onearm/check tc/if-twoarm/check))
  
  (define-signature tc-lambda^
    (tc/lambda tc/lambda/check tc/rec-lambda/check))
  
  (define-signature tc-app^
    (tc/app tc/app/check))
  
  (define-signature tc-let^
    (tc/let-values tc/letrec-values tc/let-values/check tc/letrec-values/check))
  
  )