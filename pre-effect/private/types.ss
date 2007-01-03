(module types mzscheme
  
  (require  (lib "struct.ss")
            (lib "match.ss")
            (lib "list.ss")
            (lib "trace.ss")
            (all-except (lib "unit.ss") rename)
            "type-def-structs.ss"
            "signatures.ss"
            "planet-requires.ss"
            "type-printer-unit.ss"
            "big-unit.ss")
  
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))  
  (require-libs)
  
  

  (define-syntax ->
    (syntax-rules ()
      [(_ dom ... rng)
       (make-funty (list (make-arr* (list dom ...) rng)))]))
  (define-syntax ->*
    (syntax-rules ()
      [(_ dom rng)
       (make-funty (list (make-arr* dom rng)))]
      [(_ dom rst rng)
       (make-funty (list (make-arr* dom rng rst)))]))
  (define-syntax cl->
    (syntax-rules ()
      [(_ [(dom ...) rng] ...)
       (make-funty (list (make-arr* (list dom ...) rng) ...))]))
  
  
  
  
  (provide-signature-elements type-printer^)
  (require "type-structs.ss")
  
  (provide (all-defined)
           (all-from-except "type-structs.ss" make-arr)
           (rename make-arr* make-arr))  

  
  (define-syntax define/match
    (syntax-rules ()
      [(_ nm cl ...) (define nm (match-lambda* cl ...))]))
  
  (define make-arr*
    (case-lambda [(dom rng) (make-arr* dom rng #f)]
                 [(dom rng rest) (make-arr dom rng rest)]))
  
  (define (make-poly* tvars t)
    (if (null? tvars) t (make-poly tvars t)))
  
  (define N (make-base-type 'number))
  (define B (make-base-type 'boolean))
  (define Sym (make-base-type 'symbol))
  (define Void (make-base-type 'void))
  (define Bytes (make-base-type 'bytes))
  (define String (make-base-type 'string))
  (define Keyword (make-base-type 'keyword))
  
  (define Univ (make-univ))
  (define Dyn (make-dynamic))
  

  
  )