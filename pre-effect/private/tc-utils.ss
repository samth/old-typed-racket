(module tc-utils mzscheme
  (provide (all-defined))
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))

  ;; a parameter representing the original location of the syntax being currently checked
  (define current-orig-stx (make-parameter #'here))
  
  (define (find-origin stx)
    (cond [(syntax-property stx 'origin) => (lambda (orig)
                                              (if (pair? orig) (car orig) orig))]
          [else #f]))
  
  ;; produce a type error, using the current syntax
  (define (tc-error msg . rest)
    (define cur-stx (current-orig-stx))
    (define new-stx (if (identifier? cur-stx) cur-stx (datum->syntax-object cur-stx '... cur-stx cur-stx)))
    (raise-syntax-error 'typecheck (apply format msg rest) new-stx new-stx))
  
  (define (tc-error/stx stx msg . rest)
    (parameterize ([current-orig-stx stx])
      (apply tc-error msg rest)))
  
  ;; check two identifiers to see if they have the same name
  (define (symbolic-identifier=? a b)
    (eq? (syntax-e a) (syntax-e b)))
  
  ;; parameter for currently-defined type aliases
  (define current-type-names (make-parameter (lambda () '())))
      


  
  )