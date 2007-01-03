(module subtype mzscheme
  (require "types.ss" "type-equal.ss" "infer.ss" "planet-requires.ss" "subst.ss")
  (require (lib "67.ss" "srfi"))
  
  (require-libs)
  
  
  ;; exn representing failure of subtyping
  ;; s,t both types
  (define-struct (exn:subtype exn:fail) (s t))
  
  (define (empty-set) (set:make-ordered (lambda (x y) (pair-compare type-compare type-compare x y))))
  
  
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
  ;; set[(cons type type)] type type -> maybe[set[(cons type type)]]
  (define (subtype*/no-fail A s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (subtype* A s t)))
  
  ;; type type -> (does not return)
  ;; subtying fails
  (define (fail! s t) (raise (make-exn:subtype "subtyping failed" (current-continuation-marks) s t)))
  
  ;; check subtyping for two lists of types
  ;; set[(cons type type)] listof[type] listof[type] -> set[(cons type type)]
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
  
  ;; versions of these functions that allow the predicate to return any value
  (define (set:all?* f s) (set:all? (lambda (t) (not (not (f t)))) s))
  (define (set:any?* f s) (set:any? (lambda (t) (not (not (f t)))) s))
  
  ;; simple co/contra-variance for ->
  (define (arr-subtype*/no-fail A0 s t)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])
      (match (list s t)
        [(($ arr s1 s2 #f) ($ arr t1 t2 #f))
         (let ([A1 (subtypes* A0 t1 s1)])
           (subtype* A1 s2 t2))]
        [(($ arr s1 s2 s3) ($ arr t1 t2 t3))
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
        [else (error "match failure!!!" s t (arr? s) (arr? t))])))
  
  (define (subtypes/varargs args dom rst)
    (with-handlers
        ([exn:subtype? (lambda _ #f)])      
      (subtypes*/varargs (empty-set) args dom rst)))
  
  (define (subtypes*/varargs A0 argtys dom rst)
    (let loop ([dom dom] [argtys argtys] [A A0])
      (define (succ A) A)
      (cond
        [(and (null? dom) (null? argtys)) A]
        [(null? argtys) (fail! argtys dom)]
        [(and (null? dom) rst)
         (cond [(subtype* A (car argtys) rst)]
               [else (fail! (car argtys) rst)])]
        [(null? dom) (fail! argtys dom)]
        [(subtype* A (car argtys) (car dom))]
        [else (fail! (car argtys) (car dom))])))
             
            
         
  
  (require (planet "memoize.ss" ("dherman" "memoize.plt" 2 0)))
  
  ;; the algorithm for recursive types transcribed directly from TAPL, pg 305
  ;; set[(cons type type)] type type -> set[(cons type type)]
  ;; potentially raises exn:subtype, when the algorithm fails
  ;; is s a subtype of t, taking into account constraints A
  (define/memo* (subtype* A s t)
     
    (parameterize ([match-equality-test type-equal?])
      (if (set:member? (cons s t) A) A
          (let* ([A0 (set:insert (cons s t) A)])
            (match (list s t)
              ;; subtyping is reflexive
              [(t t) A0]
              [(($ pred-ty t1) ($ pred-ty t2)) (subtype* A0 t1 t2)]
              ;; translated pred types into function types
              [(($ pred-ty t) other) (subtype* A0 (make-arr (list Univ) B) other)]
              [(other ($ pred-ty t)) (subtype* A0 other (make-arr (list Univ) B))]
              [(($ value v1) ($ value v2)) (=> unmatch) (if (equal? v1 v2) A0 (unmatch))]
              ;; values are subtypes of their "type"
              [(($ value (? number? n)) ($ base-type 'number)) A0]
              [(($ value (? boolean? n)) ($ base-type 'boolean)) A0]
              [(($ value (? symbol? n)) ($ base-type 'symbol)) A0]
              [(($ value (? string? n)) ($ base-type 'string)) A0]
              ;; tvars are equal if they are the same variable
              [(($ tvar t) ($ tvar t*)) (if (eq? t t*) A0 (fail! s t))]
              ;; univ is top
              [(_ ($ univ)) A0]
              ;; dynamic is both top and bottom
              [(or (($ dynamic) _) (_ ($ dynamic))) A0]
              ;; case-lambda
              [(($ funty (arr1 ...)) ($ funty (arr2 ...)))
               (when (null? arr1) (fail! s t))
               (let loop ([A* A0]
                          [arr2 arr2])
                 (cond 
                   [(null? arr2) A*]
                   [(supertype-of-one/arr A* (car arr2) arr1) => (lambda (A) (loop A (cdr arr2)))]
                   [else (fail! s t)]))]
              ;; recur structurally on pairs
              [(($ pair-ty a d) ($ pair-ty a* d*))
               (let ([A1 (subtype* A0 a a*)])
                 (and A1 (subtype* A1 d d*)))]
              ;; quantification over two types preserves subtyping
              [(($ poly n b1) ($ poly m b2)) (subtype* A0 b1 (subst m (make-tvar n) b2))]
              ;; use unification to see if we can use the polytype here
              [(($ poly v b) s) (=> unmatch)
                                (if (unify1 s b) A0 (unmatch))]
              [(s ($ poly vs b)) (=> unmatch)
                                 (if (set:empty? (fv b)) (subtype* A0 s b) (unmatch))]
              ;; just unfold the recursive types
              [(_ ($ mu x t1)) (subtype* A0 s (unfold t))]
              [(($ mu x s1) _) (subtype* A0 (unfold s) t)]
              ;; for unions, we check the cross-product
              [(($ union es) t) (and (set:all?* (lambda (elem) (subtype* A0 elem t)) es) A0)]
              [(s ($ union es)) (and (set:any?* (lambda (elem) (subtype*/no-fail A0 s elem)) es) A0)]
              ;; subtyping on structs follows the declared hierarchy
              [(($ struct-ty nm (? type? parent) flds) other) (subtype* A0 parent other)]
              ;; subtyping on values is pointwise
              [(($ values-ty vals1) ($ values-ty vals2)) (subtypes* A0 vals1 vals2)]
              ;; single values shouldn't actually happen, but they're just like the type
              [(t ($ values-ty (t*))) (printf "BUG - singleton values type~n") (subtype* A0 t t*)]
              [(($ values-ty (t)) t*) (printf "BUG - singleton values type~n") (subtype* A0 t t*)]
              ;; otherwise, not a subtype
              [_ (fail! s t) (printf "failed")])))))
  
  (define (type-compare? a b)
    (and (subtype a b) (subtype b a)))
  
  
  
  (provide (all-defined))
  
  ;(trace subtype*)
  ;(trace subtype-of-one/arr)
  ;(trace arr-subtype*/no-fail)
  ;(trace subtype-of-one)
  ;(trace subtype*/no-fail)
  ;(trace subtypes*)
  ;(trace subtype)
  
  ;(subtype (-> Univ B) (-> Univ Univ))
  ;(subtype (make-poly '(a) (make-tvar 'a)) (make-lst N))
  
  
  )