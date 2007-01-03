(module test-utils mzscheme
  (provide (all-defined))

  (define-syntaxes (require/private require-for-syntax/private)
    (let ()
      (define ((mk r) stx)
        (syntax-case stx ()
          [(_ . args)
           (let ([reqs
                  (map (lambda (i) (string-append "../private/" (symbol->string (syntax-e i)) ".ss")) 
                       (syntax->list #'args))])
             (datum->syntax-object stx `(,r ,@reqs)))]))
      (values (mk #'require) (mk #'require-for-syntax))))

  (require/private planet-requires type-equal)
  
  (require-libs)
  (require-schemeunit)
  
  (define (mk-suite ts)
    (match (map (lambda (f) (f)) ts)
      [(t) t]
      [ts (apply test-suite "Combined Test Suite" ts)]))

  (define (run . ts)
    (test/text-ui (mk-suite ts)))
  
  (define (run/gui . ts)
    (test/graphical-ui (mk-suite ts)))
  

  (define-syntax (define-go stx)
    (syntax-case stx ()
      [(_ args ...)
       (with-syntax 
           ([go (datum->syntax-object stx 'go)]
            [go/gui (datum->syntax-object stx 'go/gui)]
            [(tmps ...) (generate-temporaries #'(args ...))])
         #'(define-values (go go/gui)
             (let ([tmps args] ...)
               (values (lambda () (run tmps ...))
                       (lambda () (run/gui tmps ...))))))]))
  
  (define-syntax (check-type-equal? stx)
    (syntax-case stx ()
      [(_ nm a b)
       #`(test-check nm type-equal? a b)]))
    
)