(module type-equal mzscheme
  (require (all-except (lib "unit.ss") rename) "big-unit.ss" "signatures.ss")
  (provide-signature-elements type-equal^)
  )