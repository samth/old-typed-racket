(module rep-utils mzscheme
   
  (require (lib "plt-match.ss"))
  (require (lib "struct.ss")
           (lib "boundmap.ss" "syntax"))
  (require-for-syntax (lib "struct.ss" "syntax") (lib "stx.ss" "syntax") "utils.ss")
  (require "planet-requires.ss")
  (require-galore)
  
  (provide == dt de print-type* print-effect* Type Type? Effect Effect? defintern hash-id Type-seq Effect-seq)
  
  (define-match-expander
    ==
    (lambda (stx)
      (syntax-case stx ()
        [(_ val)
         #'(? (lambda (x) (equal? val x)))])))
  
  (define-for-syntax printing? #t)
  
  (define print-type* (box (lambda _ (error "print-type* not yet defined"))))
  (define print-effect* (box (lambda _ (error "print-effect* not yet defined"))))
    
  ;; Name = Symbol
  
  (define-syntax (define-struct/printer stx)
    (syntax-case stx ()
      [(form name (flds ...) printer)
       #`(define-struct/properties name (flds ...) 
           #,(if printing? #'([prop:custom-write printer]) #'())
           #f)]))
  
  (define-values (Covariant Contravariant Invariant Constant)
    (let ()
      (define-struct Variance () #f)
      (define-struct (Covariant Variance) () #f)
      (define-struct (Contravariant Variance) () #f)
      (define-struct (Invariant Variance) () #f)
      (define-struct (Constant Variance) () #f)
      (values (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant))))
  
  
  (provide Covariant Contravariant Invariant Constant)
  
  ;; hashtables for keeping track of free variables and indexes
  (define index-table (make-hash-table 'weak))
  ;; maps Type to List[Cons[Number,Variance]]
  (define var-table (make-hash-table 'weak))
  ;; maps Type to List[Cons[Symbol,Variance]]
  
  (define (free-idxs* t) (hash-table-get index-table t (lambda _ (error "type not in index-table" t))))
  (define (free-vars* t) (hash-table-get var-table t (lambda _ (error "type not in var-table" t))))
  
  ;; all types are Type?
  (define-struct/printer Type (seq) (lambda (a b c) ((unbox print-type*) a b c)))
  
  (define-struct/printer Effect (seq) (lambda (a b c) ((unbox print-effect*) a b c)))
  
  (define-syntax defintern
    (syntax-rules ()
      [(_ name+args make-name key) 
       (defintern name+args (lambda () (make-hash-table #;'weak 'equal)) make-name key)]
      [(_ (*name arg ...) make-ht make-name key-expr)
       (define *name
         (let ([table (make-ht)])
           (lambda (arg ...)
             #;(all-count!)
             (let ([key key-expr])
               (hash-table-get table key
                               (lambda ()
                                 (let ([new (make-name (count!) arg ...)])
                                   (hash-table-put! table key new)
                                   new)))))))]))
  
  (define (make-count!)
    
    (let ([state 0])
      (lambda () (begin0 state (set! state (add1 state)))))   
    #;
    (let ([ch (make-channel)])
      (thread (lambda () (let loop ([n 0]) (channel-put ch n) (loop (add1 n)))))
      (lambda () (channel-get ch))))
  
  (provide count! all-count! union-count!)
  
  (define count! (make-count!))
  (define union-count! (make-count!))
  (define all-count! (make-count!))
  (define id-count! (make-count!))
  
  (define-for-syntax (id kw . args)
    (define (f v)
      (cond [(string? v) v]
            [(symbol? v) (symbol->string v)]
            [(identifier? v) (symbol->string (syntax-e v))]))
    (datum->syntax-object kw (string->symbol (apply string-append (map f args)))))
  
  (define identifier-table (make-module-identifier-mapping))
  
  (define (hash-id id)
    (module-identifier-mapping-get 
     identifier-table 
     id 
     (lambda () (let ([c (id-count!)])
                  (module-identifier-mapping-put! identifier-table id c)
                  c))))
  
  (require (lib "define-struct.ss" "big")
           (lib "list.ss" "srfi" "1")
           (lib "etc.ss"))
  
  (provide free-vars* free-idxs* empty-hash-table make-invariant)
  
  ;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
  ;; (listof frees) -> frees
  (define (combine-frees freess)    
    (define ht (make-hash-table))
    (define (combine-var v w)
      (cond
        [(eq? v w) v]
        [(eq? v Constant) w]
        [(eq? w Constant) v]
        [else Invariant]))
    (for-each
     (lambda (old-ht)
       (hash-table-for-each 
        old-ht
        (lambda (sym var)
          (let* ([sym-var (hash-table-get ht sym (lambda () #f))])
            (if sym-var
                (hash-table-put! ht sym (combine-var var sym-var))
                (hash-table-put! ht sym var))))))
     freess)
    ht)
    
  ;; frees -> frees
  (define (flip-variances vs)
    (hash-table-map* 
     (lambda (k v) 
       (evcase 
        v
        [Covariant Contravariant]
        [Contravariant Covariant]
        [else v]))
     vs))
  
  (define (make-invariant vs)
    (hash-table-map* 
     (lambda (k v) Invariant)
     vs))
  
  (define (hash-table-map* f ht)
    (define new-ht (hash-table-copy ht))
    (hash-table-for-each 
     new-ht
     (lambda (k v)
       (hash-table-put! 
        new-ht
        k
        (f k v))))
    new-ht)
  
  (define (without-below n frees)
    (define new-ht (hash-table-copy frees))
    (hash-table-for-each 
     new-ht
     (lambda (k v)
       (when (< k n) (hash-table-remove! new-ht k))))
    new-ht)
  
  (provide combine-frees flip-variances without-below)
  
  (define-syntax (unless-in-table stx) 
    (syntax-case stx ()
      [(_ table val . body)
       (quasisyntax/loc stx
         (hash-table-get table val #,(syntax/loc #'body (lambda () . body))))]))
  
  (define empty-hash-table (make-immutable-hash-table null))
  
  (define-syntaxes (dt de)
    (let ()
      (define (parse-opts opts stx)
        (let loop ([provide? #t] [intern? #f] [frees #t] [opts opts])
          (cond 
            [(null? opts) (values provide? intern? frees)]
            [(eq? #:no-provide (syntax-e (stx-car opts)))
             (loop #f intern? frees (cdr opts))]
            [(eq? #:no-frees (syntax-e (stx-car opts)))
             (loop #f intern? #f (cdr opts))]
            [(not (and (stx-pair? opts) (stx-pair? (stx-car opts))))
             (raise-syntax-error #f "bad options" stx)]
            [(eq? #:intern (syntax-e (stx-car (car opts))))
             (loop provide? (stx-car (stx-cdr (car opts))) frees (cdr opts))]
            [(eq? #:frees (syntax-e (stx-car (car opts))))
             (loop provide? intern? (stx-cdr (car opts)) (cdr opts))]
            [else (raise-syntax-error #f "bad options" stx)])))
      (define (mk par)        
        (lambda (stx)          
          (syntax-case stx ()
            [(_ nm flds . opts)
             (let-values ([(provide? intern? frees) (parse-opts (syntax->list #'opts) #'opts)])
               (with-syntax* ([ex (id #'nm #'nm ":")]
                              [parent par]
                              [(_ maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds) #f #t #'nm)]
                              [(flds* ...) #'flds]
                              [*maker (id #'nm "*" #'nm)]
                              [**maker (id #'nm "**" #'nm)]
                              [provides (if provide? 
                                            #`(begin 
                                                (provide ex pred acc ...)
                                                (provide (rename *maker maker)))
                                            #'(begin))]
                              [intern (cond 
                                        [(syntax? intern?)
                                         #`(defintern (**maker . flds) maker #,intern?)]                                        
                                        [(null? (syntax-e #'flds))
                                         #'(defintern (**maker . flds) maker #f)]
                                        [(stx-null? (stx-cdr #'flds)) #'(defintern (**maker . flds) maker . flds)]
                                        [else #'(defintern (**maker . flds) maker (list . flds))])]
                              [frees (cond
                                       [(not frees) #'(begin)]
                                       ;; we know that this has no free vars
                                       [(and (pair? frees) (syntax? (car frees)) (not (syntax-e (car frees))))
                                        (syntax/loc stx
                                          (define (*maker . flds)
                                            (define v (**maker . flds)) 
                                            #;(printf  "~a entered in #f case~n" '*maker)
                                            (unless-in-table 
                                             var-table v
                                             (hash-table-put! var-table v empty-hash-table)
                                             (hash-table-put! index-table v empty-hash-table))
                                            v))]
                                       ;; we provided an expression each for calculating the free vars and free idxs
                                       ;; this should really be 2 expressions, one for each kind
                                       [(and (pair? frees) (pair? (cdr frees)))
                                        (quasisyntax/loc
                                            stx
                                          (define (*maker . flds)
                                            (define v (**maker . flds))
                                            #;(printf  "~a entered in expr case ~n~a~n~a ~n" '*maker '#,(car frees) '#,(cadr frees))
                                            #,
                                            (quasisyntax/loc (car frees)
                                              (unless-in-table 
                                               var-table v
                                               (hash-table-put! var-table v #,(car frees))
                                               (hash-table-put! index-table v #,(cadr frees))))
                                            #;(printf  "~a exited in expr case~n" '*maker)
                                            v))]                                       
                                       [else 
                                        (let 
                                            ([combiner 
                                              (lambda (f flds)
                                                (syntax-case flds ()
                                                  [() #'empty-hash-table]
                                                  [(e) #`(#,f e)]
                                                  [(e ...) #`(combine-frees (list (#,f e) ...))]))])
                                        (quasisyntax/loc stx
                                               (define (*maker . flds)
                                                 (define v (**maker . flds))
                                                 #;(printf  "~a entered in default case~n" '*maker)
                                                 (unless-in-table 
                                                  var-table v
                                                  (define fvs #,(combiner #'free-vars* #'flds))
                                                  (define fis #,(combiner #'free-idxs* #'flds))
                                                  (hash-table-put! var-table v fvs)
                                                  (hash-table-put! index-table v fis))
                                                 v)))])])
                 #'(begin
                     (define-struct (nm parent) flds #f)
                     (define-match-expander ex
                       (lambda (s)
                         (... 
                          (syntax-case s ()
                            [(__ fs ...) (syntax/loc s (struct nm (_ fs ...)))]))))
                     intern
                     provides
                     frees)))])))
      (values (mk #'Type) (mk #'Effect))))
  
  )