(module effect-rep mzscheme
  
  (require (lib "plt-match.ss"))
  (require (lib "etc.ss"))
  (require "rep-utils.ss")
  
  (de True-Effect () [#:frees #f])
  
  (de False-Effect () [#:frees #f])
  
  ;; v is an identifier
  (de Var-True-Effect (v) [#:intern (hash-id v)] [#:frees #f])
  #;(defintern (**Var-True-Effect v) make-Var-True-Effect (hash-id v))

  ;; v is an identifier
  (de Var-False-Effect (v) [#:intern (hash-id v)] [#:frees #f])
  #;(defintern (**Var-False-Effect v) make-Var-False-Effect (hash-id v))
  
  ;; t is a Type
  ;; v is an identifier
  (de Restrict-Effect (t v) [#:intern (list t (hash-id v))] [#:frees (free-vars* t) (free-idxs* t)])
  #;(defintern (**Restrict-Effect t v) make-Restrict-Effect)
  
  ;; t is a Type
  ;; v is an identifier
  (de Remove-Effect (t v) [#:intern (list t (hash-id v))] [#:frees (free-vars* t) (free-idxs* t)])
  #;(defintern (**Remove-Effect t v) make-Remove-Effect (list t (hash-id v)))

  ;; t is a Type
  (de Latent-Restrict-Effect (t))
  
  ;; t is a Type
  (de Latent-Remove-Effect (t))

  (de Latent-Var-True-Effect () [#:frees #f])

  (de Latent-Var-False-Effect () [#:frees #f])

  ;; could also have latent true/false effects, but seems pointless
  

  )
