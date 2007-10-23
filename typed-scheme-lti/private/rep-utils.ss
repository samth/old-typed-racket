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
  
  (define-syntaxes (dt de)
    (let ()
      (define (parse-opts opts)
        (values (not (member #:no-provide opts))
                (not (member #:no-intern opts))))
      (define (mk par)
        (lambda (stx)
          (syntax-case stx ()
            [(_ nm . flds)
             (andmap identifier? (syntax->list #'flds))
             ((mk par) #'(_ nm flds))]
            [(_ nm flds . opts)
             (let-values ([(provide? intern?) (parse-opts (syntax-object->datum #'opts))])
               (with-syntax* ([ex (id #'nm #'nm ":")]
                              [parent par]
                              [(_ maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds) #f #t #'nm)]
                              [*maker (id #'nm "*" #'nm)]
                              [provides (if provide? 
                                            #`(begin 
                                                (provide ex pred acc ...)
                                                (provide (rename *maker maker)))
                                            #'(begin))]
                              [intern (cond 
                                        [(not intern?) #'(begin)]
                                        [(null? (syntax-e #'flds))
                                         #'(defintern (*maker . flds) maker #f)]
                                        [(stx-null? (stx-cdr #'flds)) #'(defintern (*maker . flds) maker . flds)]
                                        [else #'(defintern (*maker . flds) maker (list . flds))])])
                 #'(begin
                     (define-struct (nm parent) flds #f)
                     (define-match-expander ex
                       (lambda (s)
                         (... 
                          (syntax-case s ()
                            [(__ fs ...) (syntax/loc s (struct nm (_ fs ...)))]))))
                     intern
                     provides)))])))
      (values (mk #'Type) (mk #'Effect))))
  
  )