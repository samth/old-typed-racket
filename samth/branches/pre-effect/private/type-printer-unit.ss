(module type-printer-unit mzscheme
  
  (require "signatures.ss" (lib "unit.ss") "planet-requires.ss" "type-def-structs.ss" "tc-utils.ss" )
  (require-libs)
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))
  
  (provide (all-defined))
  
  (define-unit type-printer@
    (import type-structs^ infer^ subst^)
    (export type-printer^)
    
    (define print-poly-types? #f)
    (define print-aliases #f)
    
    
    (define (has-name t)    
      (define ns ((current-type-names)))
      (define (lookup v l)
        (let ([x (assoc v l)])
          (if x (cadr x) v)))
      (let/cc return
        (for-each
         (lambda (pair)
           (cond [(and print-aliases (equiv? t (cdr pair)))
                  (return (car pair))]
                 [(and 
                   print-poly-types?
                   (poly? (cdr pair))
                   ;; fix in case when variable names are the same!
                   (unify1 (poly-type (cdr pair)) t))
                  => (lambda (s)
                       #;(printf "succeded in unifying! ~a~n" s)
                       (return
                        #;(printf "vars are: ~a~n subst-vars are:~a~n" (poly-var (cdr pair)) (map car s))
                          (let ([args (map (lambda (v) (lookup v s))
                                           (poly-var (cdr pair)))])
                            `(,(car pair) ,@args))))]))
         ns)
        #f))
    
    
    
    
    (define (print-type c port write?)
      (define (fp . args) (apply fprintf port args))    
      (cond 
        [(has-name c) => (lambda (x) (fp "~a" x))]
        [(base-type? c) (fprintf port "~a" (base-type-name c))]
        [(struct-ty? c) (fprintf port "#<struct:~a ~a>" (struct-ty-name c) (struct-ty-flds c))]
        [(pred-ty? c) (fprintf port "(pred ~a)" (pred-ty-type c))]
        [(arr? c) (fprintf port "(")
                  (for-each (lambda (t) (fprintf port "~a " t)) (arr-dom c))
                  (if (arr-rest c)
                      (fp "~a .. " (arr-rest c)))
                  (fprintf port "-> ~a)" (arr-rng c))]
        [(funty? c) (let ([arities (funty-arities c)])
                      (if (= 1 (length arities))
                          (fprintf port "~a" (car arities))
                          (begin
                            (fprintf port "(case-lambda ")
                            (for-each (lambda (x) (fp "~a " x)) (funty-arities c))
                            (fp ")"))))]                        
        [(value? c) (let ([v (value-v c)])
                      (cond [(or (symbol? v) (null? v))
                             (fp "'~a" v)]
                            [else (fp "~a" v)]))]
        [(vec? c) (fprintf port "(vector-of ~a)" (vec-elem c))]
        [(union? c) (fprintf port "~a" (cons 'Un (set:elements (union-elems c))))]
        [(univ? c) (fprintf port "top")]
        [(pair-ty? c) (fp "(cons ~a ~a)" (pair-ty-car c) (pair-ty-cdr c))]
        [(dynamic? c) (fp "*")]
        [(tvar? c) (fprintf port "<~a>" (tvar-name c))]
        [(poly? c) (fprintf port "(All ~a. ~a)" (poly-var c) (poly-type c))]
        [(mu? c) (fprintf port "(mu ~a ~a)" (mu-var c) (mu-type c))]
        [(values-ty? c) (fp "~a" (cons 'values (values-ty-types c)))]
        )))
  )