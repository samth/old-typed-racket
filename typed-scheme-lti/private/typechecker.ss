(module typechecker mzscheme
  (require "unit-utils.ss"
           (lib "trace.ss")
           (only (lib "unit.ss") provide-signature-elements)
            "signatures.ss" "typechecker-unit.ss"
            "tc-if-unit.ss" "tc-lambda-unit.ss" "tc-app-unit.ss"
            "tc-let-unit.ss")
  
  (provide-signature-elements typechecker^)
  
  (define-values/link-units/infer
    typechecker@ tc-if@ tc-lambda@ tc-app@ tc-let@)
    
  )