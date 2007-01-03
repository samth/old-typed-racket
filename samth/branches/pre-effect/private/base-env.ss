(module base-env mzscheme
  
  (require-for-template (only (lib "list.ss") foldl)
                        mzscheme
                        #;"prims.ss"
                        "extra-procs.ss")
  
  #;(require-for-syntax (lib "list.ss")
                      (only (lib "1.ss" "srfi") partition)
                      "types.ss"
                      "types-aux.ss")
  
  (require
   "types.ss"
   (only (lib "list.ss") foldl)
   #;"prims.ss"
   "types-aux.ss")
     
  (provide initial-env)
  
  (define-syntax (make-env stx)
    (syntax-case stx ()
      [(_ (nm ty) ...)
       #`(list (list #'nm ty) ...)]))
  
  
  (define-syntax (make-base-env stx)
    (syntax-case stx ()
      [(_ initial-env (nm ty) ...)
       #'(begin
           (define initial-env (list (list #'nm ty) ...)))]))

  (define initial-env
    (let ([make-lst make-Listof]
          [make-lst/elements -pair])
      (make-env
       
       (car (make-poly (list 'a 'b) (cl-> [((make-lst (make-tvar 'a))) (make-tvar 'a)]
                                          [((make-pair-ty (make-tvar 'a) (make-tvar 'b))) (make-tvar 'a)])))
       (cadr (make-poly (list 'a) (-> (make-lst (make-tvar 'a)) (make-tvar 'a))))
       (cdr (make-poly (list 'a 'b) (cl-> [((make-lst (make-tvar 'a))) (make-lst (make-tvar 'a))]
                                          [((make-pair-ty (make-tvar 'a) (make-tvar 'b))) (make-tvar 'b)])))
       (cddr (make-poly (list 'a) (-> (make-lst (make-tvar 'a)) (make-lst (make-tvar 'a)))))
       (cons (make-poly (list 'a 'b) 
                        (cl-> [((make-tvar 'a) (make-lst (make-tvar 'a))) (make-lst (make-tvar 'a))]
                              [((make-tvar 'a) (make-tvar 'b)) (make-pair-ty (make-tvar 'a) (make-tvar 'b))])))
       
       (null? (make-pred-ty (make-value null)))
       [null (make-value null)]
       (atom? (make-pred-ty A))
       (number? (make-pred-ty N))
       (boolean? (make-pred-ty B))
       (add1 (-> N N))
       (sub1 (-> N N))
       (eq? (-> Univ Univ B))
       (eqv? (-> Univ Univ B))
       (equal? (-> Univ Univ B))
       (even? (-> N B))
       [not (-> B B)]
       [cons? (make-pred-ty (-pair Univ Univ))]
       [pair? (make-pred-ty (-pair Univ Univ))]
       [empty? (make-pred-ty (make-value null))]
       [string? (make-pred-ty String)]
       [map (make-poly '(a b c)
                       (cl-> [((-> (make-tvar 'a) (make-tvar 'b))
                               (make-lst (make-tvar 'a)))
                              (make-lst (make-tvar 'b))]
                             [((-> (make-tvar 'a) (make-tvar 'b) (make-tvar 'c))
                               (make-lst (make-tvar 'a))
                               (make-lst (make-tvar 'b)))
                              (make-lst (make-tvar 'c))]))]
       [foldl (make-poly '(a b) 
                         (-> (-> (make-tvar 'a) (make-tvar 'b) (make-tvar 'b)) (make-tvar 'b) (make-lst (make-tvar 'a)) (make-tvar 'b)))]
       
       
       
       (error (cl->
               [() Dyn] 
               [(Sym String) Dyn] 
               [(Sym)  Dyn] 
               [(String)  Dyn]
               [(String Univ)  Dyn]
               [(Sym String Univ)  Dyn]))
       (match:error (Dyn . -> . Dyn))
       (display (Univ . -> . Void))
       [void (->* '() Univ Void)]
       [printf (->* (list String) Univ Void)]
       [format (->* (list String) Univ String)]   
       (fst (make-poly (list 'a 'b) (-> (make-lst/elements (make-tvar 'a) (make-tvar 'b)) (make-tvar 'a))))
       (snd (make-poly (list 'a 'b) (-> (make-lst/elements (make-tvar 'a) (make-tvar 'b)) (make-tvar 'b))))
       
       (= (->* (list N N) N B))
       (>= (->* (list N N) N B))
       (zero? (N . -> . B))
       (* (->* '() N N))
       (/ (->* (list N) N N))
       (+ (->* '() N N))
       (- (->* (list N) N N))
       (max (->* (list N) N N))
       [values  (make-poly '(a) (-> (make-tvar 'a) (make-tvar 'a)))]
       [vector-ref 
        (make-poly (list 'a) ((make-vec (make-tvar 'a)) N . -> . (make-tvar 'a)))]
       [build-vector (make-poly (list 'a) (N (N . -> . (make-tvar 'a)) . -> . (make-vec (make-tvar 'a))))]
       [reverse (make-poly '(a) (-> (make-lst (make-tvar 'a)) (make-lst (make-tvar 'a))))]
       [append (make-poly '(a) (-> (make-lst (make-tvar 'a)) (make-lst (make-tvar 'a)) (make-lst (make-tvar 'a))))]
       [length (make-poly '(a) (-> (make-lst (make-tvar 'a)) N))]
       [memq (make-poly (list 'a) (-> (make-tvar 'a) (make-lst (make-tvar 'a)) (Un (make-value #f) (make-lst (make-tvar 'a)))))]
       [memv (make-poly (list 'a) (-> (make-tvar 'a) (make-lst (make-tvar 'a)) (Un (make-value #f) (make-lst (make-tvar 'a)))))]
       [member (make-poly (list 'a) (-> (make-tvar 'a) (make-lst (make-tvar 'a)) (Un (make-value #f) (make-lst (make-tvar 'a)))))]
       [> (-> N N B)])))

  )