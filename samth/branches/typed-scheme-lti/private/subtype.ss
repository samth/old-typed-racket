(module subtype mzscheme
  (require #;"type-effect-convenience.ss" "type-rep.ss" "unify.ss"
           "tc-utils.ss"
           "type-comparison.ss")
  (require (lib "plt-match.ss")
           (lib "trace.ss"))
  
  
  
  ;; exn representing failure of subtyping
  ;; s,t both types
  
  (define-struct (exn:subtype exn:fail) (s t))
  #;
  (define-values (fail-sym exn:subtype?)
    (let ([sym (gensym)])
      (values sym (lambda (s) (eq? s sym)))))
  
  ;; inference failure - masked before it gets to the user program
  (define-syntax fail!
    (syntax-rules ()
      [(_ s t) #;(raise fail-sym)
       (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t))
       #;(error "inference failed" s t)]))
  
  ;; data structures for remembering things on recursive calls
  (define (empty-set) '())  
  (define (seen-before s t) (cons (Type-seq s) (Type-seq t)))
  (define (remember s t A) (cons (seen-before s t) A))
  (define (seen? s t A) (member (seen-before s t) A))
  
  
  ;; is s a subtype of t?
  ;; type type -> boolean
  (define (subtype s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (subtype* (empty-set) s t)))
  
  ;; are all the s's subtypes of all the t's?
  ;; [type] [type] -> boolean
  (define (subtypes s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (subtypes* (empty-set) s t)))
  
  ;; subtyping under constraint set, but produces boolean result instead of raising exn
  ;; List[(cons Number Number)] type type -> maybe[List[(cons Number Number)]]
  (define (subtype*/no-fail A s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (subtype* A s t)))
  
  ;; type type -> (does not return)
  ;; subtying fails
  #;
  (define (fail! s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t)))
  
  ;; check subtyping for two lists of types
  ;; List[(cons Number Number)] listof[type] listof[type] -> List[(cons Number Number)]
  (define (subtypes* A ss ts)
    (cond [(and (null? ss) (null? ts) A)]
          [(or (null? ss) (null? ts)) (fail! ss ts)]
          [(subtype* A (car ss) (car ts)) 
           =>
           (lambda (A*) (subtypes* A* (cdr ss) (cdr ts)))]
          [else (fail! (car ss) (car ts))]))
  
  ;; check if s is a supertype of any element of ts
  (define (supertype-of-one/arr A s ts)
    (ormap (lambda (e) (arr-subtype*/no-fail A e s)) ts))
  
  
  ;; simple co/contra-variance for ->
  (define (arr-subtype*/no-fail A0 s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (match (list s t)
        [(list (arr: s1 s2 #f thn-eff els-eff) (arr: t1 t2 #f (or '() thn-eff) (or '() els-eff)))
         (let ([A1 (subtypes* A0 t1 s1)])
           (subtype* A1 s2 t2))]
        [(list (arr: s1 s2 s3 thn-eff els-eff) (arr: t1 t2 t3 (or '() thn-eff) (or '() els-eff)))
         ;; either the effects have to be the same, or the supertype can't have effects
         (let ()
           (define (succ A)
             (if (not t3) (subtype* A s2 t2)
                 (let ([A1 (subtype* A t3 s3)])
                   (subtype* A1 s2 t2))))
           (succ (subtypes*/varargs A0 t1 s1 s3)))
         #;(let loop ([s1 s1] [t1 t1] [A A0])
             (define (succ A)
               (if (not t3) (subtype* A s2 t2)
                   (let ([A1 (subtype* A t3 s3)])
                     (subtype* A1 s2 t2))))
             (cond
               [(and (null? s1) (null? t1)) (succ A)]
               [(and (null? s1) s3)
                (cond [(subtype* A (car t1) s3) => (lambda (A) (loop s1 (cdr t1) A))]
                      [else (fail!)])]
               [(subtype* A (car t1) (car s1)) => (lambda (A) (loop (cdr s1) (cdr t1) A))]
               [else (fail!)]))]
        [else (fail! s t) #;(error "match failure!!!" s t (arr? s) (arr? t))])))
  
  (define (subtypes/varargs args dom rst)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])      
      (subtypes*/varargs (empty-set) args dom rst)))
  
  (define (subtypes*/varargs A0 argtys dom rst)
    (let loop-varargs ([dom dom] [argtys argtys] [A A0])
      (define (succ A) A)
      (cond
        [(and (null? dom) (null? argtys)) A]
        [(null? argtys) (fail! argtys dom)]
        [(and (null? dom) rst)
         (cond [(subtype* A (car argtys) rst) => (lambda (A) (loop-varargs dom (cdr argtys) A))]
               [else (fail! (car argtys) rst)])]
        [(null? dom) (fail! argtys dom)]
        [(subtype* A (car argtys) (car dom)) => (lambda (A) (loop-varargs (cdr dom) (cdr argtys) A))]
        [else (fail! (car argtys) (car dom))])))
  
             
  ;; the algorithm for recursive types transcribed directly from TAPL, pg 305
  ;; List[(cons Number Number)] type type -> List[(cons Number Number)]
  ;; potentially raises exn:subtype, when the algorithm fails
  ;; is s a subtype of t, taking into account constraints A
  (define (subtype* A s t)     
    (parameterize ([match-equality-test type-equal?])
      (if (seen? s t A) A
          (let* ([A0 (remember s t A)])
            (match (list s t)
              ;; subtyping is reflexive
              [(list t t) A0]
              ;; pred
              #;[(($ pred-ty t1) ($ pred-ty t2)) (subtype* A0 t1 t2)]
              ;; translated pred types into function types
              #;[(($ pred-ty t) other) (subtype* A0 (make-arr (list Univ) B) other)]
              #;[(other ($ pred-ty t)) (subtype* A0 other (make-arr (list Univ) B))]
              [(list (Value: v1) (Value: v2)) (=> unmatch) (if (equal? v1 v2) A0 (unmatch))]
              ;; values are subtypes of their "type"
              [(list (Value: (? number? n)) (Base: 'Number)) A0]
              [(list (Value: (? boolean? n)) (Base: 'Boolean)) A0]
              [(list (Value: (? symbol? n)) (Base: 'Symbol)) A0]
              [(list (Value: (? string? n)) (Base: 'String)) A0]
              ;; tvars are equal if they are the same variable
              [(list (F: t) (F: t*)) (if (eq? t t*) A0 (fail! s t))]
              ;; univ is top
              [(list _ (Univ:)) A0]
              ;; case-lambda
              [(list (Function: arr1) (Function: arr2))
               (when (null? arr1) (fail! s t))
               (let loop-arities ([A* A0]
                                  [arr2 arr2])
                 (cond 
                   [(null? arr2) A*]
                   [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop-arities A (cdr arr2)))]
                   [else (fail! s t)]))]
              ;; recur structurally on pairs
              [(list (Pair: a d) (Pair: a* d*))
               (let ([A1 (subtype* A0 a a*)])
                 (and A1 (subtype* A1 d d*)))]
              ;; quantification over two types preserves subtyping
              [(list (Poly: ns b1) (Poly: ms b2)) 
               (=> unmatch)
               (unless (= (length ns) (length ms)) 
                 (unmatch))
               ;(printf "Poly: ~n~a ~n~a~n" b1 (subst-all (map list ms (map make-F ns)) b2))
               (subtype* A0 b1 (subst-all (map list ms (map make-F ns)) b2))]
              ;; use unification to see if we can use the polytype here
              [(list (Poly: vs b) s)
               (=> unmatch)
               (if (unify1 s b) A0 (unmatch))]
              [(list s (Poly: vs b))
               (=> unmatch)
               (if (null? (fv b)) (subtype* A0 s b) (unmatch))]
              ;; just unfold the recursive types
              [(list _ (? Mu?)) (subtype* A0 s (unfold t))]
              [(list (? Mu?) _) (subtype* A0 (unfold s) t)]
              ;; for unions, we check the cross-product
              [(list (Union: es) t) (and (andmap (lambda (elem) (subtype* A0 elem t)) es) A0)]
              [(list s (Union: es)) (and (ormap (lambda (elem) (subtype*/no-fail A0 s elem)) es) A0)]
              ;; subtyping on structs follows the declared hierarchy
              [(list (Struct: nm (? Type? parent) flds proc) other) (subtype* A0 parent other)]
              ;; Promises are covariant
              [(list (Struct: 'Promise _ (list t) _) (Struct: 'Promise _ (list t*) _)) (subtype* A0 t t*)]
              ;; subtyping on values is pointwise
              [(list (Values: vals1) (Values: vals2)) (subtypes* A0 vals1 vals2)]
              ;; single values shouldn't actually happen, but they're just like the type
              [(list t (Values: (list t*))) (int-err "BUG - singleton values type~a" (make-Values (list t*)))]
              [(list (Values: (list t)) t*) (int-err "BUG - singleton values type~a" (make-Values (list t)))]
              ;; otherwise, not a subtype
              [_ (fail! s t) (printf "failed")])))))
  
  (define (type-compare? a b)
    (and (subtype a b) (subtype b a)))
  
  (provide subtype type-compare? subtypes/varargs subtypes)
  
  ;(trace subtype*)
  ;(trace supertype-of-one/arr)
  ;(trace arr-subtype*/no-fail)
  ;(trace subtype-of-one)
  ;(trace subtype*/no-fail)
  ;(trace subtypes*)
  ;(trace subtype)
  
  ;(subtype (-> Univ B) (-> Univ Univ))
  ;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))
  
  
  )