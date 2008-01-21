(module type-contract mzscheme
  
  (provide type->contract)
  
  (require
   "type-rep.ss"   
   "parse-type.ss"
   "utils.ss"      
   "type-name-env.ss"
   "require-contract.ss"
   "internal-forms.ss"
   "tc-utils.ss"
   "resolve-type.ss"
   "type-utils.ss"
   (lib "datatype.ss" "CSU660"))
  
  (require
   (lib "plt-match.ss")
   (lib "struct.ss" "syntax")
   (lib "stx.ss" "syntax")
   (lib "trace.ss")
   (only (lib "contract.ss") -> ->* case-> cons/c)
   (lib "etc.ss")           
   (lib "struct.ss")
   #;(lib "syntax-browser.ss" "macro-debugger"))
  
  (require-for-template mzscheme (lib "contract.ss"))
  
  
  
  (define (type->contract ty fail)
    (let/cc exit
      (let t->c ([ty ty])
        (match ty
          [(or (App: _ _ _) (Name: _)) (t->c (resolve-once ty))]
          [(Univ:) #'any/c]
          ;; we special-case lists:
          [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
           #`(listof-unsafe #,(t->c elem-ty))]
          [(Base: sym)
           (case sym
             [(Number) #'number?]
             [(Boolean) #'boolean?]
             [(Keyword) #'keyword?]
             [(Port) #'port?]
             [(Path) #'path?]
             [(String) #'string?]
             [(Symbol) #'symbol?]
             [(Bytes) #'bytes?]
             [(Void) #'void?]
             [(Syntax) #'syntax?]
             [(Output-Port) #'output-port?]
             [(Input-Port) #'input-port?]
             [(Char) #'char?]
             [(Namespace) #'namespace?]
             [else (int-err "Base type ~a cannot be converted to contract" sym)])]
          [(Union: elems) 
           (with-syntax 
               ([cnts (map t->c elems)])
             #;(printf "~a~n" (syntax-object->datum #'cnts))
             #'(or/c . cnts))]
          [(Function: arrs)
           (let ()
             (define (f a)
               (define-values (dom* rngs* rst)
                 (match a
                   [(arr: dom (Values: rngs) #f _ _)
                    (values (map t->c dom) (map t->c rngs) #f)]
                   [(arr: dom rng #f _ _)
                    (values (map t->c dom) (list (t->c rng)) #f)]
                   [(arr: dom (Values: rngs) rst _ _)
                    (values (map t->c dom) (map t->c rngs) (t->c rst))]
                   [(arr: dom rng rst _ _)
                    (values (map t->c dom) (list (t->c rng)) (t->c rst))]))
               (with-syntax 
                   ([(dom* ...) dom*]
                    [(rng* ...) rngs*]
                    [rst* rst])
                 (if rst
                     #'((dom* ...) (listof rst*) . ->* . (rng* ...))
                     #'((dom* ...) . ->* . (rng* ...)))))
             (let ([l (map f arrs)])
               (if (and (pair? l) (null? (cdr l)))
                   (car l)
                   #`(case-> #,@l))))]
          [(Vector: t)
           #`(vectorof #,(t->c t))]
          [(Pair: t1 t2)
           #`(cons-unsafe/c #,(t->c t1) #,(t->c t2))]
          [(Opaque: p? cert)
           #`(flat-contract #,(cert p?))]
          [(Value: #f) #'false/c]      
          [(Value: '()) #'null?]
          [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x #,v)))]
          [else (exit (fail))]))))
  
  )