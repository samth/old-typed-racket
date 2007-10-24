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
             (let ([key key-expr])
               (hash-table-get table key
                               (lambda ()
                                 (let ([new (make-name (count!) arg ...)])
                                   (hash-table-put! table key new)
                                   new)))))))]))
  
  (define count!
    
    (let ([state 0])
      (lambda () (begin0 state (set! state (add1 state)))))   
    #;
    (let ([ch (make-channel)])
      (thread (lambda () (let loop ([n 0]) (channel-put ch n) (loop (add1 n)))))
      (lambda () (channel-get ch))))
  
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
     (lambda () (let ([c (count!)])
                  (module-identifier-mapping-put! identifier-table id c)
                  c))))
  
  (require (lib "define-struct.ss" "big"))
  
  (define-syntaxes (dt de)
    (let ()
      (define (parse-opts opts)
        (let loop ([provide? #t] [intern? #t] [frees #t] [opts opts])
          (cond 
            [(null? opts) (values provide? intern? frees)]
            [(eq? #:no-provide (syntax-e (car opts)))
             (loop #f intern? frees (cdr opts))]
            [(eq? #:no-intern (syntax-e (car opts)))
             (loop provide? #f frees (cdr opts))]
            [(eq? #:frees (syntax-e (car opts)))
             (loop provide? intern? (cadr opts) (cddr opts))]
            [else (raise-syntax-error #f "bad options")])))
      (define (mk par)        
        (lambda (stx)          
          (syntax-case stx ()
            [(_ nm flds . opts)
             (let-values ([(provide? intern? frees) (parse-opts (syntax->list #'opts))])
               (with-syntax* ([ex (id #'nm #'nm ":")]
                              [parent par]
                              [(_ maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds) #f #t #'nm)]
                              [(flds* ...) #'flds]    
                              [(tmp-maker) (generate-temporaries #'(maker))]
                              [*maker (id #'nm "*" #'nm)]
                              [provides (if provide? 
                                            #`(begin 
                                                (provide ex pred acc ...)
                                                (provide (rename *maker maker)))
                                            #'(begin))]
                              [intern (cond 
                                        [(not intern?) #'(begin)]
                                        [(null? (syntax-e #'flds))
                                         #'(defintern (tmp-maker . flds) maker #f)]
                                        [(stx-null? (stx-cdr #'flds)) #'(defintern (tmp-maker . flds) maker . flds)]
                                        [else #'(defintern (tmp-maker . flds) maker (list . flds))])]
                              [frees (cond
                                       [(and (syntax? frees) (not (syntax-e frees)))
                                        #'(begin)]
                                       [(syntax? frees) frees]
                                       [else #'(define (*make . flds)
                                                 (define fvs (append (free-vars* flds*) ...))
                                                 (define fis (append (free-idxs* flds*) ...))
                                                 (define v (tmp-maker . flds))
                                                 (hash-table-put! var-table v fvs)
                                                 (hash-table-put! index-table v fis)
                                                 v)])])
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