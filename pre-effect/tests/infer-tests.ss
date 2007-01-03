(module infer-tests mzscheme
  (require "test-utils.ss")
  (require/private planet-requires infer types types-aux)
  (require-schemeunit)
  (require-libs)
  
  (provide fv-tests)
  
  (define-syntax fv-t
    (syntax-rules ()
      [(_ ty elems ...)
       (let ([ty* ty])
         (test-check (format "~a" ty*)
                     set:equal?
                     (fv ty*)
                     (set:make-eq (quote elems) ...)))]))
  
  (define (fv-tests)
    (test-suite "Tests for fv"
                (fv-t N)
                [fv-t (-v a) a]
                [fv-t (-poly (a) a)]
                [fv-t (-poly (a b c d e) a)]
                [fv-t (-poly (b) (-v a)) a]
                [fv-t (-poly (b c d e) (-v a)) a]
                [fv-t (-mu a (-lst a))]
                [fv-t (-mu a (-lst (-pair a (-v b)))) b]
                ))
  
  (define-go fv-tests)
  )