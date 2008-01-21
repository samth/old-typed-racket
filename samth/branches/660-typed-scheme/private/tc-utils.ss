(module tc-utils mzscheme
  (provide (all-defined))
  (require (lib "list.ss") (lib "etc.ss"))

  ;; a parameter representing the original location of the syntax being currently checked
  (define current-orig-stx (make-parameter #'here))
  
  ;; helper function, not currently used
  (define (find-origin stx)
    (cond [(syntax-property stx 'origin) => (lambda (orig)
                                              (let ([r (reverse orig)])
                                                (let loop ([r (reverse orig)])
                                                  (if (null? r) #f
                                                      (if (syntax-source (car r)) (car r)
                                                          (loop (cdr r)))))))]
          [else #f]))
  
  ;; do we print the fully-expanded syntax in error messages?
  (define print-syntax? (make-parameter #t))
  
  
  (define check-unreachable-code? (make-parameter #f))
  
  ;; produce a type error, using the current syntax
  (define (tc-error msg . rest)
    (define cur-stx (current-orig-stx))
    (define new-stx 
      (cond 
        [(or (print-syntax?)
             (syntax-original? cur-stx)
             (identifier? cur-stx))
         cur-stx]
        [(and (not (syntax-source cur-stx)) (find-origin cur-stx))]
        [else (datum->syntax-object cur-stx (syntax-e (or (find-origin cur-stx) #'..)) cur-stx cur-stx)]))
    (raise-syntax-error 'typecheck (apply format msg rest) new-stx new-stx))
  
  ;; produce a type error, given a particular syntax
  (define (tc-error/stx stx msg . rest)
    (parameterize ([current-orig-stx stx])
      (apply tc-error msg rest)))
  
  ;; check two identifiers to see if they have the same name
  (define (symbolic-identifier=? a b)
    (eq? (syntax-e a) (syntax-e b)))
  
  ;; parameter for currently-defined type aliases
  ;; this is used only for printing type names
  (define current-type-names (make-parameter (lambda () '())))
  
  ;; error for unbound variables
  (define (lookup-fail e) (tc-error "unbound identifier ~a" e))  


  ;; for reporting internal errors in the type checker
  (define-struct (exn:fail:tc exn:fail) ())
  
  ;; raise an internal error - typechecker bug!
  (define (int-err msg . args) 
    (raise (make-exn:fail:tc (string-append "Internal Typechecker Error: " (apply format msg args))
                             (current-continuation-marks))))
  
  (define-syntax (nyi stx)
    (syntax-case stx ()
      [(_ str)
       (quasisyntax/loc stx (int-err "~a: not yet implemented: ~a" str #,(syntax/loc stx (this-expression-file-name))))]
      [(_) (syntax/loc stx (nyi ""))]))

  
  ;; are we currently expanding in a typed module (or top-level form)?
  (define typed-context? (box #f))
  
  ;; what type names have been referred to in this module?
  (define type-name-references (make-parameter '()))
  
  (define (add-type-name-reference t)
    (type-name-references (cons t (type-name-references))))
  
  )