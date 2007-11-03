;(require (lib "debug.ss""macro-debugger" "model"))
;(trace-verbose? #t)
; (trace #'
(module meet-join mzscheme
  
  (require "union.ss" "type-rep.ss"
           "subtype.ss"
           "type-effect-convenience.ss"
           "planet-requires.ss"
           (lib "plt-match.ss"))
  
  (require-schemeunit)
  
  (provide meet join promote demote)
  (define (join a b) (Un a b))
  
  (define (meet a b)
    (cond [(type-equal? a b) a]
          [(subtype a b) a]
          [(subtype b a) b]
          [else (Un)]))
  
  (define (promote t V)
    (define (sb t) (promote t V))
    (if (zero? (hash-table-count (free-vars* t)))
        t
        (match t
          [(F: (? (lambda (v) (memq v V)))) Univ]
          [(Vector: _) Univ]
          [(Box: _) Univ]
          [(Pair: a b) (make-Pair (promote a V) (promote b V))]
          [_ t])))
  
  (define (demote t V)
    (if (zero? (hash-table-count (free-vars* t)))
        t
        (match t
          [(F: (? (lambda (v) (memq v V)))) (Un)]
          [(Vector: _) (Un)]
          [(Box: _) (Un)]
          [_ t])))
 
  
  )
;)