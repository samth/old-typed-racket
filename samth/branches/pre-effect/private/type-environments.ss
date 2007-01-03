(module type-environments mzscheme
  
  (provide (all-defined))
  
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0))
           "tc-utils.ss" 
           "types.ss" "types-aux.ss"
           "subst.ss"
           (lib "list.ss"))
  
  ;; error for unbound variables
  (define (lookup-fail e) (tc-error "unbound identifier ~a" e))  
  
 ;; the initial type name environment - just the base types
  (define-syntax (define-tname-env stx)
    (syntax-case stx ()
      [(_ var [nm ty] ...)
       #'(begin
           (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
           (define var
             (list (list #'nm ty) ...)))]))  
  
  
  (define-tname-env initial-type-names
    [atom A]
    [number N]
    [boolean B]
    [symbol Sym]
    [str String]
    [top Univ]
    #;[sexp Sexp]
    [num-exp NE]
    #;[aexp AE]
    [Option (-poly (a) (Un (make-value #f) a))]
    [Sexp -Sexp]
    [List (-mu x (Un (make-value '()) (-pair Univ x)))]
    [Listof -Listof]
    [list-of -Listof]
    )
  
  ;; the initial type variable environment - empty
  (define initial-tvar-env
    (symbol-env))

  ;; a parameter for the current type variables
  (define current-tvars (make-parameter initial-tvar-env))  
  
  ;; some utility functions for environments
  ;;

  ;; extend that works on single arguments
  (define (extend env k v) (extend-env (list k) (list v) env))
  
  ;; takes two lists of sets to be added, which are either added one at a time, if the 
  ;; elements are not lists, or all at once, if the elements are lists
  (define (extend/values kss vss env)
    (foldr (lambda (ks vs env) (cond [(and (list? ks) (list? vs))                                      
                                      (extend-env ks vs env)]
                                     [(or (list? ks) (list? vs))
                                      (tc-error "not both lists in extend/values")]
                                     [else (extend-env (list ks) (list vs) env)])) 
           env kss vss))
  
  ;; extend-env with the environment first.  pretty pointless, really
  (define (extend-multiple env ids types)
    (extend-env ids types env))


  )