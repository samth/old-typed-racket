(module require-contract mzscheme
  (require (lib "contract.ss"))
  (provide require/contract)
  (define-syntax (require/contract stx)
    (syntax-case stx ()
      [(require/contract nm cnt lib)
       #`(begin (require (rename lib tmp nm))
                (define nm (contract cnt tmp '#,(syntax-object->datum #'nm) 'never-happen #'#,stx)))]))
 )
#|
(module a mzscheme
  (provide x)
  (define (x a) 'hi))

(module z mzscheme
  (require require-contract)
  
  (require (lib "contract.ss"))
  
  (require/contract x (number? . -> . number?) a )
  
  (x 'no)
  )

(require z)
|#