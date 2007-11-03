(module type-rep mzscheme
  
  (require (lib "plt-match.ss"))
  (require (lib "etc.ss") (lib "list.ss"))
  (require "planet-requires.ss" "rep-utils.ss" "effect-rep.ss" "tc-utils.ss")
  (require (prefix 1: (lib "list.ss" "srfi" "1"))
           (lib "trace.ss"))
  
  (define name-table (make-hash-table 'weak))
  
  ;; Name = Symbol
  
  ;; Type is defined in rep-utils.ss
  
  ;; t must be a Type
  (dt Scope (t))
  
  ;; i is an nat
  (dt B (i) [#:frees empty-hash-table (make-immutable-hash-table (list (cons i Covariant)))])
  
  ;; n is a Name
  (dt F (n) [#:frees (make-immutable-hash-table (list (cons n Covariant))) empty-hash-table])
  
  ;; left and right are Types
  (dt Pair (left right))
  
  ;; elem is a Type
  (dt Vector (elem) [#:frees (make-invariant (free-vars* elem)) (make-invariant (free-idxs* elem))])
  
  ;; elem is a Type
  (dt Box (elem) [#:frees (make-invariant (free-vars* elem)) (make-invariant (free-idxs* elem))])  
  
  ;; name is a Symbol (not a Name)
  (dt Base (name) [#:frees #f])
  
  ;; body is a Scope
  (dt Mu (body) #:no-provide [#:frees (free-vars* body) (without-below 1 (free-idxs* body))])    
  
  ;; n is how many variables are bound here
  ;; body is a Scope
  (dt Poly (n body) #:no-provide [#:frees (free-vars* body) (without-below n (free-idxs* body))])
  
  ;; pred : identifier
  ;; cert : syntax certifier
  (dt Opaque (pred cert) [#:intern (hash-id pred)] [#:frees #f])
  #;(defintern (*Opaque pred cert) make-Opaque (hash-id pred))
  
  ;; name : symbol
  ;; parent : Struct
  ;; flds : Listof[Type]
  ;; proc : Function Type
  (dt Struct (name parent flds proc) 
      [#:frees (combine-frees (map free-vars* (append (if proc (list proc) null) (if parent (list parent) null) flds)))
               (combine-frees (map free-idxs* (append (if proc (list proc) null) (if parent (list parent) null) flds)))])
  
  ;; dom : Listof[Type]
  ;; rng : Type
  ;; rest : Type
  ;; thn-eff : Effect
  ;; els-eff : Effect
  ;; arr is NOT a Type
  (dt arr (dom rng rest thn-eff els-eff)
      [#:frees (combine-frees (append (map flip-variances (map free-vars* dom)) 
                                      (map free-vars* (append (list rng) 
                                                              (if rest (list rest) null)
                                                              thn-eff
                                                              els-eff))))
               (combine-frees (append (map flip-variances (map free-idxs* dom))
                                      (map free-idxs* (append (list rng) 
                                                              (if rest (list rest) null)
                                                              thn-eff
                                                              els-eff))))])
  
  ;; arities : Listof[arr]
  (dt Function (arities) [#:frees (combine-frees (map free-vars* arities))
                                  (combine-frees (map free-idxs* arities))])
  
  ;; v : Scheme Value
  (dt Value (v) [#:frees #f])
  
  ;; elems : Listof[Type]
  (dt Union (elems) [#:frees (combine-frees (map free-vars* elems))
                             (combine-frees (map free-idxs* elems))])
  
  (dt Univ () [#:frees #f])
  
  ;; types : Listof[Type]
  (dt Values (types) [#:frees (combine-frees (map free-vars* types))
                              (combine-frees (map free-idxs* types))])
  
  ;; in : Type
  ;; out : Type
  (dt Param (in out))
  
  ;; key : Type
  ;; value : Type
  (dt Hashtable (key value))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Ugly hack - should use units
  
  (define union-maker (box #f))
  
  (define (set-union-maker! v) (set-box! union-maker v))
  
  (provide set-union-maker!)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; remove-dups: List[Type] -> List[Type]
  ;; removes duplicate types from a SORTED list
  (define (remove-dups types)
    (cond [(null? types) types]
          [(null? (cdr types)) types]
          [(type-equal? (car types) (cadr types)) (remove-dups (cdr types))]
          [else (cons (car types) (remove-dups (cdr types)))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  
  ;; sub-eff : (Type -> Type) Eff -> Eff
  (define (sub-eff sb eff) 
    (match eff
      [(Restrict-Effect: t v) (make-Restrict-Effect (sb t) v)]
      [(Remove-Effect: t v) (make-Remove-Effect (sb t) v)]
      [(Latent-Restrict-Effect: t) (make-Latent-Restrict-Effect (sb t))]
      [(Latent-Remove-Effect: t) (make-Latent-Remove-Effect (sb t))]
      [(Latent-Var-True-Effect:) eff]
      [(Latent-Var-False-Effect:) eff]
      [(True-Effect:) eff]
      [(False-Effect:) eff]
      [(Var-True-Effect: v) eff]
      [(Var-False-Effect: v) eff]))
  
  ;; varChanger : (Num Name -> Type) (Num Num -> Type) Type -> Type  
  (define-syntax (varChanger* stx)
    (define (mk type-expr outer is-F key f-name f-expr b-name b-expr)
      (with-syntax ([outer outer]
                    [free-f* (if is-F #'free-vars* #'free-idxs*)])
        (quasisyntax/loc stx          
          (let loop ([outer 0] [ty #,type-expr])
            (define (sb t) (loop outer t))
            (let ([key-var #,key])
              (match ty
                #,@(if is-F #'([(? (lambda (t) (not (hash-table-get (free-f* t) key-var #f)))) ty]) #'())
                ;; Variables
                ;; Free vars
                [(F: #,f-name)    #,f-expr]
                ;; Bound vars
                [(B: #,b-name)      #,b-expr]
                ;; Recursive Cases
                [(Pair: l r)   (*Pair (loop outer l) (loop outer r))]
                [(Vector: elem)   (*Vector (loop outer elem))]
                [(Hashtable: k v) (*Hashtable (loop outer k) (loop outer v))]
                [(Param: in out) (*Param (loop outer in) (loop outer out))]
                [(Struct: nm par fs proc)
                 (*Struct nm par 
                          (map sb fs) 
                          (if proc (loop outer proc) proc))]
                [(Union: elems) (*Union (remove-dups (sort (map sb elems) type<?)))]
                [(Values: vs) (*Values (map sb vs))]
                ;; Functions
                [(Function: arities)
                 (*Function (map (match-lambda
                                   [(arr: ins out rest thn-eff els-eff)
                                    (*arr (map sb ins)
                                          (sb out)
                                          (if rest (sb rest) #f)
                                          (map (lambda (e) (sub-eff sb e)) thn-eff)
                                          (map (lambda (e) (sub-eff sb e)) els-eff))])
                                 arities))]
                ;; special case for lists
                [(Mu: (Scope: (Union: (list (Value: '()) (Pair: elem (B: 0))))))
                 (*Mu (*Scope (*Union (list (*Value null) (*Pair (loop (add1 outer) elem) (*B 0))))))]
                ;; Binding Forms
                [(Mu: (Scope: body)) 
                 (*Mu (*Scope (loop (add1 outer) body)))]
                [(Poly: n body*)
                 (let ([body (remove-scopes n body*)])
                   (*Poly n (*Scope (loop (+ n outer) body))))]
                ;; Trivial Cases
                [(Base: _)   ty]
                [(Opaque: _ _)   ty]
                [(Value: _)   ty]
                [(Univ:)   ty]
                ))))))
    
    (syntax-case stx ()
      [(form ty #:idx key outer arg expr)
       (mk #'ty #'outer #f #'key
           #'f-name #'(*F f-name)
           #'arg #'expr)]
      [(form ty #:var key outer arg expr)
       (mk #'ty #'outer #t #'key
           #'arg #'expr
           #'b-name #'(*B b-name))]))
  
  (define (varChanger F-op B-op ty)        
    (define (loop outer ty)
      (define (sb t) (loop outer t))
      (match ty
        ;; Variables
        ;; Free vars
        [(F: name*)    (F-op outer name*)]
        ;; Bound vars
        [(B: idx)      (B-op outer idx)]
        ;; Recursive Cases
        [(Pair: l r)   (*Pair (loop outer l) (loop outer r))]
        [(Vector: elem)   (*Vector (loop outer elem))]
        [(Hashtable: k v) (*Hashtable (loop outer k) (loop outer v))]
        [(Param: in out) (*Param (loop outer in) (loop outer out))]
        [(Struct: nm par fs proc)
         (*Struct nm par 
                  (map sb fs) 
                  (if proc (loop outer proc) proc))]
        [(Union: elems) (*Union (remove-dups (sort (map sb elems) type<?)))]
        [(Values: vs) (*Values (map sb vs))]
        ;; Functions
        [(Function: arities)
         (*Function (map (match-lambda
                           [(arr: ins out rest thn-eff els-eff)
                            (*arr (map sb ins)
                                  (sb out)
                                  (if rest (sb rest) #f)
                                  (map (lambda (e) (sub-eff sb e)) thn-eff)
                                  (map (lambda (e) (sub-eff sb e)) els-eff))])
                         arities))]
        ;; Binding Forms
        [(Mu: (Scope: body)) 
         (*Mu (*Scope (loop (add1 outer) body)))]
        [(Poly: n body*)
         (let ([body (remove-scopes n body*)])
           (*Poly n (*Scope (loop (+ n outer) body))))]
        ;; Trivial Cases
        [(Base: _)   ty]
        [(Opaque: _ _)   ty]
        [(Value: _)   ty]
        [(Univ:)   ty]
        ))
    (loop 0 ty))
  
  (define (add-scopes n t)
    (if (zero? n) t
        (add-scopes (sub1 n) (*Scope t))))
  
  (define (remove-scopes n sc)
    (if (zero? n) 
        sc
        (match sc
          [(Scope: sc*) (remove-scopes (sub1 n) sc*)]
          [_ (int-err "Tried to remove too many scopes: ~a" sc)])))

  ;; abstract-many : Names Type -> Scope^n 
  ;; where n is the length of names  
  (define (abstract-many names ty)
    (define (nameTo name count type)
      
      (varChanger* type #:var name outer name*
                   (if (eq? name name*)
                       (*B (+ count outer))
                       (*F name*)))      
      #;
      (varChanger (lambda (outer name*)
                    (if (eq? name name*)
                        (*B (+ count outer))
                        (*F name*)))
                  (lambda (outer idx) (*B idx))
                  type))
    #;(trace nameTo)
    (let ([n (length names)])
      (let loop ([ty ty] [names names] [count (sub1 n)])
        (if (zero? count)
            (add-scopes n (nameTo (car names) 0 ty))
            (loop (nameTo (car names) count ty)
                  (cdr names)
                  (sub1 count))))))
  
  ;; instantiate-many : List[Type] Scope^n -> Type 
  ;; where n is the length of types  
  (define (instantiate-many images sc)
    (define (replace image count ty)
      
      (varChanger* ty #:idx (+ count outer) outer idx
                   (if (= (+ count outer) idx)
                       image
                       (*B idx)))
      #;
      (varChanger (lambda (outer name*) (*F name*))
                  (lambda (outer idx)
                    (if (= (+ count outer) idx)
                        image
                        (*B idx)))
                  ty))
    (let ([n (length images)])
      (let loop ([ty (remove-scopes n sc)] [images images] [count (sub1 n)])
        (if (zero? count)
            (replace (car images) 0 ty)
            (loop (replace (car images) count ty)
                  (cdr images)
                  (sub1 count))))))
  
  (define (abstract name ty)
    (abstract-many (list name) ty))
  
  (define (instantiate type sc)
    (instantiate-many (list type) sc))
  
  ;; fv : Type -> Listof[Name]
  (define (fv t) (hash-table-map (free-vars* t) (lambda (k v) k)))
  
  ;; fv/list : Listof[Type] -> Listof[Name]
  (define (fv/list ts) (hash-table-map (combine-frees (map free-vars* ts)) (lambda (k v) k)))
  
  #;
  (define-values (fv fv/list)
    (let*
        ;; l accumulates the list of free variables
        ([l '()]
         [u (*Univ)]
         ;; find the free variables of a single type
         [f (lambda (ty) (varChanger (lambda (_ name) (set! l (cons name l)) u) (lambda _ u) ty))])
      (values (lambda (t)
                (f t)
                (begin0 (1:delete-duplicates! l)
                        (set! l '())))
              (lambda (ts)
                (map f ts)
                (begin0 (1:delete-duplicates! l)
                        (set! l '()))))))

  #;(trace instantiate-many abstract-many)
  
  ;; the 'smart' constructor
  (define (Mu* name body)    
    (let ([v (*Mu (abstract name body))])
      (hash-table-put! name-table v name)
      v))
  
  ;; the 'smart' destructor
  (define (Mu-body* name t)
    (match t
      [(Mu: scope)
       (instantiate (*F name) scope)]))
  
  ;; the 'smart' constructor
  (define (Poly* names body)
    (if (null? names) body
        (let ([v (*Poly (length names) (abstract-many names body))])
          (hash-table-put! name-table v names)
          v)))
  
  ;; the 'smart' destructor
  (define (Poly-body* names t)
    (match t
      [(Poly: n scope)
       (unless (= (length names) n)
         (error "Wrong number of names"))
       (instantiate-many (map *F names) scope)]))
  
  (print-struct #t)
  #;(display (equal? (Poly* '(x) (*F 'x)) (Poly* '(y) (*F 'y))))
  
  (define-match-expander Mu-unsafe:
    (lambda (stx)
      (syntax-case stx ()
        [(_ bp) #'(? Mu? (app (lambda (t) (Scope-t (Mu-body t))) bp))])))
  
  (define-match-expander Poly-unsafe:
    (lambda (stx)
      (syntax-case stx ()
        [(_ n bp) #'(? Poly? (app (lambda (t) (list (Poly-n t) (Poly-body t))) (list n bp)))])))
  
  (define-match-expander Mu:*
    (lambda (stx)
      (syntax-case stx ()
        [(_ np bp)
         #'(? Mu?
              (app (lambda (t) (let ([sym (gensym)])
                                 (list sym (Mu-body* sym t))))
                   (list np bp)))])))
  
  (define-match-expander Mu-name:
    (lambda (stx)
      (syntax-case stx ()
        [(_ np bp)
         #'(? Mu?
              (app (lambda (t) (let ([sym (hash-table-get name-table t (lambda _ (gensym)))])
                                 (list sym (Mu-body* sym t))))
                   (list np bp)))])))
  
  ;; This match expander wraps the smart constructor
  ;; names are generated with gensym
  (define-match-expander Poly:*
    (lambda (stx)
      (syntax-case stx ()
        [(_ nps bp)
         #'(? Poly?
              (app (lambda (t) 
                     (let* ([n (Poly-n t)]
                            [syms (build-list n (lambda _ (gensym)))])
                       (list syms (Poly-body* syms t))))
                   (list nps bp)))])))
  
  ;; This match expander uses the names from the hashtable  
  (define-match-expander Poly-names:
    (lambda (stx)
      (syntax-case stx ()
        [(_ nps bp)
         #'(? Poly?
              (app (lambda (t) 
                     (let* ([n (Poly-n t)]
                            [syms (hash-table-get name-table t)])
                       (list syms (Poly-body* syms t))))
                   (list nps bp)))])))
  
  
  (provide free-vars*)
  ;; substitute : Type Name Type -> Type
  (define (substitute image name target)    
    (define (sb t) (substitute image name t))
    (match target
      ;; if this variable is not free, we ignore it      
      [(? (lambda (t) (not (hash-table-get (free-vars* t) name #f)))) target]
      ;; Variables%
      ;; Free vars
      [(F: name*)    (if (eq? name* name) image target)]
      ;; Bound vars
      [(B: idx)      target]
      ;; Recursive Cases
      [(Pair: l r)   (*Pair (sb l) (sb r))]
      [(Vector: elem)   (*Vector (sb elem))]
      [(Hashtable: k v) (*Hashtable (sb k) (sb v))]
      [(Param: in out) (*Param (sb in) (sb out))]
      [(Struct: nm par fs proc)
       (*Struct nm par 
                (map sb fs) 
                (if proc (sb proc) proc))]
      [(Union: elems) ((unbox union-maker) (map sb elems))]
      [(Values: vs) (*Values (map sb vs))]
      ;; Functions
      [(Function: arities)
       (*Function (map (match-lambda
                         [(arr: ins out rest thn-eff els-eff)
                          (*arr (map sb ins)
                                (sb out)
                                (if rest (sb rest) #f)
                                (map (lambda (e) (sub-eff sb e)) thn-eff)
                                (map (lambda (e) (sub-eff sb e)) els-eff))])
                       arities))]
      [(Mu: (Scope: (Union: (list (Value: '()) (Pair: elem (B: 0))))))
       (*Mu (*Scope (*Union (list (*Value null) (*Pair (sb elem) (*B 0))))))]
      ;; Binding Forms
      [(Mu: (Scope: body)) 
       (*Mu (*Scope (sb body)))]
      [(Poly: n body*)
       (let ([body (remove-scopes n body*)])
         (*Poly n (add-scopes n (sb body))))]
      ;; Trivial Cases
      [(Base: _)   target]
      [(Opaque: _ _)   target]
      [(Value: _)   target]
      [(Univ:)   target]
      ))
  
  ;; substitute many variables
  ;; substitution = Listof[List[Name,Type]]
  ;; subst-all : substition Type -> Type
  (define (subst-all s t)
    (foldr (lambda (e acc) (substitute (cadr e) (car e) acc)) t s))
  
  
  ;; unfold : Type -> Type
  ;; must be applied to a Mu
  (define (unfold t)
    (match t
      [(Mu: sc) (instantiate t sc)]
      [_ (int-err "unfold: requires Mu type, got ~a" t)]))
  
  (define (instantiate-poly t types)
    (match t
      [(Poly:* ns body) 
       (unless (= (length types) (length ns))
         (int-err "instantiate-poly: wrong number of types: expected ~a, got ~a" (length ns) (length types)))
       (subst-all (map list ns types) body)]
      [_ (int-err "instantiate-many: requires Poly type, got ~a" t)]))
  
  
  ;; this structure represents the result of typechecking an expression
  (define-struct tc-result (t thn els) #f)
  
  (define-match-expander tc-result:
    (lambda (stx)
      (syntax-case stx ()
        [(form pt) #'(struct tc-result (pt _ _))]
        [(form pt pe1 pe2) #'(struct tc-result (pt pe1 pe2))])))
  
  ;; convenience function for returning the result of typechecking an expression
  (define ret
    (case-lambda [(t) (make-tc-result t (list) (list))]
                 [(t thn els) (make-tc-result t thn els)]))
  
  (define (subst v t e) (substitute t v e))
  
  ;; type comparison
  
  ;; equality - good
  
  (define type-equal? eq?)
  (define tc-result-equal? equal?)
  (define (effects-equal? fs1 fs2) (andmap eq? fs1 fs2))
  
  
  ;; inequality - good
  
  (define (type<? s t)
    (< (Type-seq s) (Type-seq t)))
  
  (define (type-compare s t)
    (cond [(eq? s t) 0]
          [(type<? s t) 1]
          [else -1]))
  
  ;(trace subst subst-all)
  
  (provide
   substitute subst-all ret unfold
   tc-result-t
   Mu-name: Poly-names:
   Type-seq Effect-seq  
   Mu-unsafe: Poly-unsafe:
   Mu? Poly?
   arr
   fv fv/list
   Type? Effect?
   Poly-n
   instantiate-poly
   tc-result:
   subst
   type-equal? tc-result-equal? type-compare effects-equal? type<?
   remove-dups
   (rename Mu:* Mu:)
   (rename Poly:* Poly:)
   (rename Mu* make-Mu)
   (rename Poly* make-Poly)
   (rename Mu-body* Mu-body)
   (rename Poly-body* Poly-body))
  
  ;(trace unfold)
  
  )