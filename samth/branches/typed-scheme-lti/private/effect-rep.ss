(module effect-rep mzscheme
  
  (require (lib "plt-match.ss"))
  (require (lib "etc.ss"))
  (require "rep-utils.ss")
  
  (de True-Effect)
  
  (de False-Effect)
  
  ;; v is an identifier
  (de Var-True-Effect (v) #:no-intern)
  (defintern (*Var-True-Effect v) make-Var-True-Effect (hash-id v))

  ;; v is an identifier
  (de Var-False-Effect (v) #:no-intern)
  (defintern (*Var-False-Effect v) make-Var-False-Effect (hash-id v))
  
  ;; t is a Type
  ;; v is an identifier
  (de Restrict-Effect (t v) #:no-intern)
  (defintern (*Restrict-Effect t v) make-Restrict-Effect (list t (hash-id v)))
  
  ;; t is a Type
  ;; v is an identifier
  (de Remove-Effect (t v) #:no-intern)
  (defintern (*Remove-Effect t v) make-Remove-Effect (list t (hash-id v)))

  ;; t is a Type
  (de Latent-Restrict-Effect t)
  
  ;; t is a Type
  (de Latent-Remove-Effect t)

  (de Latent-Var-True-Effect)

  (de Latent-Var-False-Effect)

  ;; could also have latent true/false effects, but seems pointless
  
  
  )
