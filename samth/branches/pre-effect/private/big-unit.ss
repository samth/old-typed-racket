(module big-unit mzscheme
  (require "unit-utils.ss"
           (only (lib "unit.ss") provide-signature-elements)
           "subst-unit.ss" "signatures.ss" "infer-unit.ss" "type-equal-unit.ss"
           "type-def-structs.ss" "type-printer-unit.ss")
       
  (provide-signature-elements infer^ type-structs^ type-printer^ subst^ type-equal^)  

  (define-values/link-units/infer
    infer@ type-structs@ type-printer@ subst@ type-equal@)  
  )