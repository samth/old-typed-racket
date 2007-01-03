(module redex-model mzscheme
  (require (planet "reduction-semantics.ss" ("robby" "redex.plt" 2 6))
           (planet "gui.ss" ("robby" "redex.plt" 2 6))
           (planet "subst.ss" ("robby" "redex.plt" 2 6))
           (lib "list.ss")
           (lib "trace.ss")
           (lib "match.ss"))
  
  (require (planet "environment.ss" ("cobbe" "environment.plt" 3 0)))
   
  
  (define occur-lang
    (language (e (e e)
                 (if e e e)
                 wrong
                 x
                 v)
              (E (E e)
                 (v E)
                 (if E e e)
                 hole)
              (v (lambda (x : t) e) number boolean c)
              (boolean #t #f)
              (t any)
              (c add1 number? boolean? zero? not)
              (x (variable-except lambda add1 if number? boolean? zero? not))))
  
  (define occur-subst
    (subst
     [`(lambda (,v : ,t) ,body)
      (all-vars (list v))
      (build (lambda (vars body) `(lambda (,@vars : ,t) ,body)))
      (subterm (list v) body)]
     [`(if ,e1 ,e2 ,e3)
       (all-vars '())
       (build (lambda (vars e1 e2 e3) `(if ,e1 ,e2 ,e3)))
       (subterm '() e1)
       (subterm '() e2)
       (subterm '() e3)]
     [(or (? boolean?) (? (lambda (x) (eq? x 'wrong))) (? number?)) (constant)]
     [(? symbol?) (variable)]
     [`(,fun ,arg)
      (all-vars '())
      (build (lambda (vars fun  arg) `(,fun ,arg)))
      (subterm '() fun)
      (subterm '() arg)]))  
  
  (define occur? (language->predicate occur-lang 'e))
  (define value? (language->predicate occur-lang 'v))
  
  (define-syntax red/context
    (syntax-rules ()
      [(_ nm . rest)
       (reduction/context/name nm occur-lang E . rest)]))

  
  ;; free-vars : e -> (listof x)
  (define free-vars
    (metafunction
     occur-lang
     [(e_1 e_2) ,(append (free-vars (term e_1))
                         (free-vars (term e_2)))]
     [x_1 ,(list (term x_1))]
     [(if e_1 e_2 e_3) ,(append (free-vars (term e_1))
                                (free-vars (term e_2))
                                (free-vars (term e_3)))]
     [(lambda (x_1) e_1)
      ,(remq* (term (x_1)) (free-vars (term e_1)))]
     [v_1 ,null]))
  
  
  (define delta
    (metafunction
     occur-lang
     [(add1 v_1) ,(if (number? (term v_1)) (add1 (term v_1))
                      (term wrong))]
     [(zero? v_1) ,(if (number? (term v_1)) (= 0 (term v_1))
                      (term wrong))]
     [(not v_1) ,(if (boolean? (term v_1)) (not (term v_1))
                     (term wrong))]
     [(number? v_1) ,(if (number? (term v_1)) (term #t) (term #f))]
     [(boolean? v_1) ,(if (boolean? (term v_1)) (term #t) (term #f))]))
  
  (define reductions
    (list
     (red/context "E-Beta"
                  ((lambda (variable_1 : t_1) e_body) v_arg)
                  (occur-subst (term variable_1)
                               (term v_arg)
                               (term e_body)))
     (red/context "E-IfTrue"
                  (if #t e_2 e_3)
                  (term e_2))
     (red/context "E-IfFalse"
                  (if #f e_2 e_3)
                  (term e_3))
     ;; for the purposes of the model, this collapes rules E-Delta and E-Wrong
     (reduction/name
      "E-Delta"
      occur-lang
      (in-hole (name E E)
               (c_op v_arg))
      (let ([v (delta (term (c_op v_arg)))])
        (if (equal? v (term wrong))
            v
            (term (in-hole E ,v)))))
     ))

  (define enable-T-IfAnd (make-parameter #f))
  (define enable-T-AbsPred (make-parameter #f))
  (define enable-T-IfTrue (make-parameter #t))
  (define enable-T-IfFalse (make-parameter #t))
  
  

  (define-struct type () #f)
  (define-struct (top-type type) () #f)
  (define-struct (base-type type) (name) #f)
  (define-struct (proc-type type) (arg result latent) #f)
  (define-struct (union-type type) (elems) #f)
  
  (define-struct effect () #f)
  (define-struct (no-effect effect) () #f)
  (define-struct (latent-type-effect effect) (t) #f)
  (define-struct (type-effect effect) (t v) #f)
  (define-struct (true-effect effect) () #f)
  (define-struct (false-effect effect) () #f)
  (define-struct (var-effect effect) (v) #f)
  
  (define N (make-base-type 'Number))
  (define B (make-base-type 'Boolean))
  (define NE (make-no-effect))
  (define Top (make-top-type))
  
  (define-struct  tc-result (type effect) #f)

  (define initial-env (symbol-env (number-var N) (boolean-var B)))

  ;; this does type intersection
  (define (type-restrict old restriction)
    (if (subtype restriction old) restriction
        (match old
          [($ union-type e) (apply Un (filter (lambda (x) (subtype x restriction)) e))]
          [_ restriction])))
  
  ;; this is set minus on types
  (define (type-minus old remove)
    (if (subtype old remove) (Un)
        (match old
          [($ union-type e) (apply Un (map (lambda (x) (type-minus x remove)) e))]
          [_ old])))
  
  (define (update-env env key f)
    (extend-env (list key) (list (f (lookup env key))) env))
  
  (define ((envop f) env eff)
    (match eff
      [($ type-effect t v) (update-env env v (lambda (old) (f old t)))]
      [_ env]))
  (define env+ (envop type-restrict))
  (define env- (envop type-minus))
  
  (define ret (case-lambda
                [(a) (make-tc-result a NE)]
                [(a b) (make-tc-result a b)]))
  
  (define ->
    (case-lambda
      [(a b) (make-proc-type a b NE)]
      [(a b c) (make-proc-type a b (make-latent-type-effect c))]))
  
  (define (Un . elems)
    (cond [(= 1 (length elems)) (car elems)]
          [else (let* ([unions (filter union-type? elems)]
                       [not-unions (filter (lambda (x) (not (union-type? x))) elems)]
                       [lists (map union-type-elems unions)]
                       [l (apply append lists)]
                       [l* (append l not-unions)])
                  (if (= 1 (length l*)) (car l*)
                      (make-union-type l*)))]))
  
  
  
  (define delta-t
    (metafunction
     occur-lang
     [add1 ,(ret (-> N N))]
     [not ,(ret (-> B B))]
     [zero? ,(ret (-> N B))]
     [number? ,(ret (-> Top B N))]
     [boolean? ,(ret (-> Top B B))]))
  
  (define-struct (exn:tc exn:fail) (stuff) #f)
  (define (fail! . args) (apply error "typechecking failed"  args))
  
  (define (parse-type t)
    (match t
      ['number N]
      ['boolean B]
      ['top Top]
      [(a '-> b '& c) (make-proc-type (parse-type a) (parse-type b) (parse-type c))]
      [('U e ...) (make-union-type (map parse-type e))]))
  
  (define (subtype a b)
    (match (list a b)
      [(a a) #t]
      [(_ ($ top-type)) #t]
      [(($ proc-type a b latent) ($ proc-type a* b* latent)) (and (subtype a* a) (subtype b b*))]
      [(($ union-type e) b) (andmap (lambda (x) (subtype x b)) e)]
      [(a ($ union-type e)) (ormap (lambda (x) (subtype a x)) e)]
      [_ #f]))
  
  ;(trace type-minus)
  ;(trace type-restrict)
  ;(trace env+)
  
  (define (null-intersect? a b)
    (match (list a b)
      [(a b) (=> unmatch) (not (or (subtype a b)
                                   (subtype b a)
                                   (unmatch)))]
      [((? base-type?) (? base-type?)) #t]
      [else #f]
      ))
 
  (define (typecheck e env)
    (define tc/local 
      (metafunction
       occur-lang
       [number ,(ret N NE)]
       [boolean_1 ,(ret B (if (term boolean_1)
                              (make-true-effect)
                              (make-false-effect)))]
       [c_1 ,(delta-t (term c_1))]
       [x_1 ,(ret (lookup env (term x_1) (lambda (x) (fail! "lookup" x))) (make-var-effect (term x_1)))]
       [(if e_test e_then e_else)
        ,(match-let* ([($ tc-result test-ty test-eff) (tc/local (term e_test))])
           
           (cond
             ;; this is slightly more aggressive in terms of the returned effect than the model
             ;; this is neccessary for typechecking tc7
             [(and (enable-T-IfFalse) (false-effect? test-eff)) (tc/local (term e_else))]
             [(and (enable-T-IfTrue) (true-effect? test-eff)) (tc/local (term e_then))]
             [else 
              (match-let* ([($ tc-result ty1 eff1) (typecheck (term e_then) (env+ env test-eff))]
                           [($ tc-result ty2 eff2) (typecheck (term e_else) (env- env test-eff))]
                           [new-eff (if (and (enable-T-IfAnd) (false-effect? eff2)) test-eff NE)])
                (ret (Un ty1 ty2) new-eff))]))]
       [(lambda (x_1 : t_1) e_body) ,(match-let* ([in (parse-type (term t_1))]
                                                  [($ tc-result out out-eff) 
                                                   (typecheck (term e_body) (extend-env (list (term x_1)) (list in) env))])
                                       (match out-eff
                                         [(and (? (lambda _ enable-T-AbsPred))                                                   
                                               ($ type-effect t v)
                                               (= type-effect-v (? (lambda (s) (eq? s (term x_1))))))
                                          (printf "got here~n")
                                          (ret (-> in out t))]
                                         [_ (ret (-> in out))]))]
       [(e_1 e_2) ,(match-let* ([($ tc-result ($ proc-type in out latent) eff) (tc/local (term e_1))]
                                [($ tc-result arg arg-eff) (tc/local (term e_2))]
                                [res-eff (cond [(and (latent-type-effect? latent)
                                                     (var-effect? arg-eff))
                                                (make-type-effect (latent-type-effect-t latent)
                                                                  (var-effect-v arg-eff))]
                                               [(and (latent-type-effect? latent)
                                                     (subtype arg (latent-type-effect-t latent)))
                                                (make-true-effect)]
                                               [(and (latent-type-effect? latent)
                                                     (null-intersect? arg (latent-type-effect-t latent)))
                                                (make-false-effect)]
                                               [else NE])])
                     (if (subtype arg in)
                         (ret out res-eff)
                         (error "argument not subtype" arg in e)))]
       [e_1 ,(fail! "unknown expression" (term e_1))]))
    (tc/local e))
  
  (define (tc e) (typecheck e initial-env))
  
  (define (check e node)
    (let* ([parents (term-node-parents node)]
           [parent-exprs (map term-node-expr parents)])
    (with-handlers ([exn:fail? (lambda _ #f)])
      (match-let* ([(($ tc-result parent-types _) ...) 
                    ;; if the parents don't typecheck, just ignore them
                    (with-handlers ([exn:fail? (lambda _ '())])
                      (map tc parent-exprs))]
                   [($ tc-result cur-type _) (tc e)])
        (andmap (lambda (t) (subtype cur-type t)) parent-types)))))
  
  (define (r t) (reduce reductions t))
  
  (define (tr t) (traces/pred occur-lang reductions (list t) check))
  
  (define t1 (term ((lambda (x : top) x) 3)))
  (define t2 (term ((lambda (x : number) (add1 x)) 3)))
  
  (define t3 (term ((lambda (x : top) (if (number? x) (add1 x) 0)) #t)))

  (define t4 (term ((lambda (x : (U number boolean)) (if (number? x) (add1 x) 0)) #t)))
  (define t5 (term ((lambda (x : (U number boolean)) (if (number? x) (zero? x) (not x))) #t)))

  ;; these use the experimental features

  ;; T-AbsPred
  (define t6 (term ((lambda (x : top) (if ((lambda (y : top) (number? y)) x) (add1 x) 0)) #t)))
  ;; T-IfAnd
  (define t7 (term ((lambda (x : top) (if (if (boolean? x) x #f) (not x) #t)) 0)))
  
  (define (trx t)
    (parameterize ([enable-T-IfAnd #t]
                   [enable-T-AbsPred #t])
      (tr t)))
  
  (print-struct #t)
  
  )