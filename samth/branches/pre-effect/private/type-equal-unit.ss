(module type-equal-unit mzscheme
  
  (require 
   "signatures.ss"
   "type-def-structs.ss"
   "planet-requires.ss"
   (lib "unit.ss")
   (lib "match.ss")
   (lib "67.ss" "srfi"))
  
  (provide (all-defined))
  
  (require-libs)
  (define-unit type-equal@
    (import type-structs^ subst^)
    (export type-equal^)
    
    (define (rename a b)
      (match* (a b)
              [($ poly vs1 b1) ($ poly vs2 b2) (let ([vs* (map (lambda (v) (make-tvar (gensym v))) vs1)])
                                                 (list (subst-all (map list vs1 vs*) b1)
                                                       (subst-all (map list vs2 vs*) b2)))]
              [($ mu v1 b1) ($ mu v2 b2) (let ([v* (make-tvar (gensym v1))])
                                           (list (subst v1 v* b1)
                                                 (subst v2 v* b2)))]))
    
    (define (poly-equal? comp? p1 p2)
      (match (list p1 p2)
        [(($ poly v1 b1) ($ poly v2 b2))
         (and (= (length v1) (length v2))
              (apply comp? (rename p1 p2)))]))
    
    (define (mu-equal? comp? m1 m2)
      (match (list m1 m2)
        [(($ mu v1 b1) ($ mu v2 b2))
         (apply comp? (rename m1 m2))]))
    
    (define type-equal-rules 
      (add-equiv-rule
       mu? mu-equal?
       (add-equiv-rule
        poly? poly-equal?
        (add-equiv-rule/leaf set:set? set:equal? default-equiv-rules))))
    
    (define type-equal? (make-equiv type-equal-rules))
    
    (current-equiv-rules type-equal-rules)
    
    (define-syntax match*
      (syntax-rules ()
        [(match* (vars ...) [pats ... expr] ...)
         (match (list vars ...)
           [(pats ...) expr] ...)]))
    
    (define (type-compare s t)
      (define (arr-ty-compare s t)
        (match (list s t)
          [(($ arr dom1 rng1 rest1) ($ arr dom2 rng2 rest2))
           (refine-compare
            (list-compare type-compare dom1 dom2)
            (type-compare rest1 rest2)
            (type-compare rng1 rng2))]))
      (define (union-elems/in-order s)
        (let* ([st (union-elems s)]
               [elems (set:elements st)])
          (sort elems (<? type-compare))))
      (select-compare 
       s t
       [dynamic? 0]
       [univ? 0]
       [tvar? (symbol-compare (tvar-name s) (tvar-name t))]
       [base-type? (symbol-compare (base-type-name s) (base-type-name t))]
       [value? (default-compare (value-v s) (value-v t))]
       [poly? (apply type-compare (rename s t))]
       [mu? (apply type-compare (rename s t))]
       [values-ty? (list-compare type-compare (values-ty-types s) (values-ty-types t))]
       [funty? (list-compare arr-ty-compare (funty-arities s) (funty-arities t))]
       [pair-ty? (type-compare (pair-ty-car s) (pair-ty-car t))
                 (type-compare (pair-ty-cdr s) (pair-ty-cdr t))]
       [vec? (type-compare (vec-elem s) (vec-elem t))]
       [pred-ty? (type-compare (pred-ty-type s) (pred-ty-type t))]
       [struct-ty? (symbol-compare (struct-ty-name s) (struct-ty-name t))
                   (type-compare (struct-ty-parent s) (struct-ty-parent t))
                   (list-compare type-compare (struct-ty-flds s) (struct-ty-flds t))]                     
       [union? (list-compare type-compare
                             (union-elems/in-order s)
                             (union-elems/in-order t))]
       ))
    
    (define (type<? a b)
      (<? type-compare a b))
    )
  
  
  
  
  )