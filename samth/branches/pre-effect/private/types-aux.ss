(module types-aux mzscheme
  (require "planet-requires.ss"
           "types.ss"
           "type-equal.ss"
           "subtype.ss")
  
  (require-libs) 

  (provide (all-defined) type-equal?)
  
  
  (define (make-union* set)
    (if (= 1 (set:size set))
        (set:select set)
        (make-union set)))
  
  (define (union2 a b)     
    (define (make-union/one t) (if (union? t) (union-elems t) (set:make-ordered type-compare t)))
    (if (subtype a b) 
        b
        (if (subtype b a) a
            (make-union* (set:union (make-union/one a) (make-union/one b))))))
  
  (define (Un . args) (foldl union2 (make-union (set:make-ordered type-compare)) args))
    
  (define-syntax -v 
    (syntax-rules ()
      [(_ x) (make-tvar 'x)]))
  
  (define-syntax -poly
    (syntax-rules ()
      [(_ (vars ...) ty)
       (let ([vars (-v vars)] ...)
         (make-poly (list 'vars ...) ty))]))
  
  (define-syntax -mu
    (syntax-rules ()
      [(_ var ty)
       (let ([var (-v var)])
         (make-mu 'var ty))]))
  
  (define -values make-values-ty)
  (define -pair make-pair-ty)
  (define -base make-base-type)
  
  (define -struct make-struct-ty)
  
  (define A (Un Sym N B String))
    
  (define (make-Listof elem) (-mu list-rec (Un (make-value null) (-pair elem list-rec))))
  (define -Listof (-poly (list-elem) (make-Listof list-elem)))
  
  (define -lst make-Listof)
  (define -val make-value)
  (define -Sexp (-mu x (Un Sym N B String (make-value '()) (-pair x x))))
  

  #;(define NE (-mu x (Un N (make-Listof x))))
  (define NE (-mu x (Un N (-pair x (-pair Sym (-pair x (-val null)))))))

  )