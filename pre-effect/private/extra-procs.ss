(module extra-procs mzscheme
  (require (lib "etc.ss"))
  (provide #;build-vector (all-defined))
  
    
    
  (define (atom? x) (or (symbol? x) (number? x)))
  
    
  (define cars car)
  (define cdrs cdr)
  (define conss cons)
  
  (define fst car)
  (define snd cdr)


  )