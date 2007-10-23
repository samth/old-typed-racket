(module tc-let-unit (lib "a-unit.ss")
  (require "signatures.ss"
           "type-effect-convenience.ss"
           "lexical-env.ss"
           "type-annotation.ss"
           "free-vars.ss"
           "utils.ss"
           (lib "trace.ss")
           (lib "kerncase.ss" "syntax"))
  
  (require-for-template (lib "plt-match.ss") "internal-forms.ss" mzscheme)
  (require-for-syntax (lib "plt-match.ss") "internal-forms.ss")
  
  (require (rename (lib "1.ss" "srfi") member1 member))
  
  
  (import typechecker^)
  (export tc-let^)
  
  (define (do-check expr->type namess types form exprs body clauses)
    ;; just for error reporting
    #;(define clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)]))
    ;; extend the lexical environment for checking the body
    (with-lexical-env/extend 
     ;; the list of lists of name
     namess
     ;; the types
     types
     (for-each (lambda (stx e t) (check-type stx (expr->type e) t))
               clauses
               exprs 
               (map list->values-ty types))
     (tc-exprs (syntax->list body))))
  
  #|
;; this is more abstract, but sucks
  (define ((mk f) namess exprs body form)
    (let* ([names (map syntax->list (syntax->list namess))]
           [exprs (syntax->list exprs)])
      (f (lambda (e->t namess types exprs) (do-check e->t namess types form exprs body)) names exprs)))
  
  (define tc/letrec-values 
    (mk (lambda (do names exprs)
          (let ([types (map (lambda (l) (map get-type l)) names)])
            (do tc-expr/t names types exprs)))))
  
  (define tc/let-values
    (mk (lambda (do names exprs)
          (let* (;; the types of the exprs
                 [inferred-types (map tc-expr/t exprs)]
                 ;; the annotated types of the name (possibly using the inferred types)
                 [types (map get-type/infer names inferred-types)])
            (do (lambda (x) x) names types inferred-types)))))
  |#
  
  (define (tc/letrec-values namess exprs body form)
    (let* ([names (map syntax->list (syntax->list namess))]
           [flat-names (apply append names)]
           [exprs (syntax->list exprs)]           
           ;; the clauses for error reporting
           [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
      (let loop ([names names] [exprs exprs] [flat-names flat-names] [clauses clauses])
        (cond 
          ;; after everything, check the body expressions
          [(null? names) (tc-exprs (syntax->list body))]
          ;; if none of the names bound in the letrec are free vars of this rhs
          [(not (ormap (lambda (n) (member1 n flat-names bound-identifier=?)) (free-vars (car exprs))))
           ;; then check this expression separately
           (let ([t (tc-expr/t (car exprs))])                 
             (with-lexical-env/extend
              (list (car names))
              (list (get-type/infer (car names) t))
              (loop (cdr names) (cdr exprs) (apply append (cdr names)) (cdr clauses))))]
          [else                
           (for-each (lambda (vs) (for-each (lambda (v) (printf/log "Letrec Var: ~a~n" (syntax-e v))) vs)) names)
           (do-check tc-expr/t names (map (lambda (l) (map get-type l)) names) form exprs body clauses)]))))
  
  (define (tc/let-values namess exprs body form)
    (let* (;; a list of each name clause
           [names (map syntax->list (syntax->list namess))]
           ;; all the trailing expressions - the ones actually bound to the names
           [exprs (syntax->list exprs)]
           ;; the types of the exprs
           [inferred-types (map tc-expr/t exprs)]
           ;; the annotated types of the name (possibly using the inferred types)
           [types (map get-type/infer names inferred-types)]
           ;; the clauses for error reporting
           [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
      (do-check (lambda (x) x) names types form inferred-types body clauses)))

  
  )