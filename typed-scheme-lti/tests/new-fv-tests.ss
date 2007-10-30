(module new-fv-tests mzscheme
  (require "test-utils.ss")
  (require/private type-rep rep-utils planet-requires)
  (require-schemeunit)  
  
  (define (random-from . args) (apply random-choice (map nonrandom args)))
  
  (define variance-gen (random-from Covariant Contravariant Invariant Constant))
  
  (define alpha-string (random-uniform string (random-char (random-int-between 65 90)) (random-size 1)))
  
  (define (free-gen var) (random-list var variance-gen))
  (define free-var-gen (free-gen (random-symbol alpha-string)))
  (define free-idx-gen (free-gen (random-size)))
  
  (define free-vars-gen (random-list-of free-var-gen))
  (define free-idxs-gen (random-list-of free-idx-gen))
  
  
  (define (fvars frees) (map car frees))
  (define (subset a b) (andmap (lambda (e) (memq e b)) a))
  
  (define (var-below v w)
    (or (eq? v w) (eq? v Invariant) (eq? w Constant)))
  
  (define (free-var-from frees)
    (random-apply car (random-apply list-ref (nonrandom frees) (random-apply sub1 (random-int-between 1 (length frees))))))

  (define (tests)
    (test-suite "random tests"
    (test-randomly "combine includes all the elements"
                   100
                   ([A free-vars-gen]
                    [B free-vars-gen]
                    [C free-idxs-gen]
                    [D free-idxs-gen])
                   (let ([C1 (combine-frees (list A B))]
                         [C2 (combine-frees (list C D))])
                     (check-not-false (subset (fvars A) (fvars C1)))
                     (check-not-false (subset (fvars B) (fvars C1)))
                     (check-not-false (subset (fvars C) (fvars C2)))
                     (check-not-false (subset (fvars D) (fvars C2)))))
    (test-randomly "combine produces lower variance"
                   100
                   
                   ([A free-vars-gen]
                    [B free-vars-gen]
                    [key (free-var-from A)])
                   (let* ([comb (combine-frees (list A B))]
                          [var1 (cadr (assq key A))]
                          [var2 (cadr (assq key comb))])
                     (check-not-false (var-below var2 var1))))))
  
  (define-go tests)
  
  
  
  )