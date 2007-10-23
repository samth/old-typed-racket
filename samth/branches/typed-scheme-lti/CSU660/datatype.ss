(module datatype mzscheme

(require "datatype-printer-props.ss"
         "../private/internal-forms.ss")
;; use mzscheme's begin (for splicing) in macro expansion, even in Lazy Scheme
(require (rename mzscheme mz:begin begin))

(provide define-type cases)

(begin-for-syntax
  ;; Utilities
  (define (syntax->lists stx)
    (map syntax->list (syntax->list stx)))
  (define (find-non-identifier xs)
    (ormap (lambda (x) (and (not (identifier? x)) x)) xs))
  (define (find-id id ids)
    (ormap (lambda (i) (and (module-identifier=? i id) i)) ids))
  (define (id prefix identifier suffix)
    (datum->syntax-object
     identifier
     (string->symbol
      (string-append prefix (symbol->string (syntax-e identifier)) suffix))
     identifier))
  (define (ids prefix identifiers suffix)
    (map (lambda (i) (id prefix i suffix)) identifiers))
  (define (list->ks l)
    (let loop ([n 0] [l l])
      (if (null? l) '() (cons n (loop (add1 n) (cdr l))))))

  ;; Sticking information on bindings
  (define-values (make-stxinfo stxinfo? stxinfo-info)
    (let-values ([(struct-info-type make-info pred getter setter)
                  (make-struct-type
                   'stxinfo #f 2 0 #f '() (current-inspector) 0)])
      (values (lambda (id info)
                (let ([rt 
                       (if id
                           (lambda (stx)
                             (syntax-case stx (set!)
                               [(set! x expr) #`(set! #,id expr)]
                               [(x . xs)      #`(#,id . xs)]
                               [x             id]))
                           (lambda (stx) (raise-syntax-error #f "Illegal use of syntax" stx)))])
                  (make-info rt info)))
              pred
              (make-struct-field-accessor getter 1 'info))))
  (define-values
    (make-*t *t? *t-type *t-type? *t-*vs set-*t-*vs!
     make-*v *v? *v-variant *v-variant? *v-*t *v-fields *v-preds *v-getters
     make-*p *p? *p-pattern *p-*v *p-*t *p-subs)
    (let ()
      ;; *t -> type info, *v -> variant info, *p -> pattern info
      (define-struct *t (type type? *vs))
      (define-struct *v (variant variant? *t fields preds getters))
      (define-struct *p (pattern *v *t subs)) ; *t is to make things faster
      (values
       make-*t *t? *t-type *t-type? *t-*vs set-*t-*vs!
       make-*v *v? *v-variant *v-variant? *v-*t *v-fields *v-preds *v-getters
       make-*p *p? *p-pattern *p-*v *p-*t *p-subs))))

(define-syntax (define-type stx)
  (syntax-case stx ()
    [(_ type [variant (field pred?) ...] ...)
     (let ([type-name     #'type]
           [variant-names (syntax->list #'(variant ...))]
           [fieldss       (syntax->lists #'((field ...) ...))]
           [predss        (syntax->lists #'((pred? ...) ...))]
           [context       (syntax-local-context)])
       ;; must be used as a top-level or module-top-level expression only
       (unless (memq context '(top-level module module-begin))
         (raise-syntax-error
          #f "can be used only as a top-level expression" stx))
       ;; check that `type' is an identifier
       (unless (identifier? type-name)
         (raise-syntax-error
          #f "expecting an identifier for the type name" stx type-name))
       ;; check that `variant's are all identifiers
       (cond [(find-non-identifier variant-names) =>
              (lambda (x)
                (raise-syntax-error
                 #f "expecting identifiers for variant names" stx x))])
       ;; check that `field's are all identifiers, and each set distinct
       (for-each
        (lambda (fields)
          (cond [(find-non-identifier fields) =>
                 (lambda (x)
                   (raise-syntax-error
                    #f "expecting identifiers for field names" stx x))]
                [(check-duplicate-identifier fields) =>
                 (lambda (dup)
                   (raise-syntax-error
                    #f "duplicate field identifier" stx dup))]))
        fieldss)
       ;; check that no variant name is the same as the type name
       (cond
        [(find-id type-name variant-names) =>
         (lambda (v)
           (raise-syntax-error
            #f "cannot use the type name for a variant" stx v))])
       (with-syntax ([type? (id "" type-name "?")]
                     [(s:variant    ...)  (ids "struct:" variant-names "")]
                     [(variant?     ...)  (ids ""        variant-names "?")]
                     [(make-variant ...)  (ids "make-"   variant-names "")]
                     [((variant-field ...) ...)
                      (map (lambda (variant fields)
                             (ids (format "~a-" (syntax-e variant)) fields ""))
                           variant-names fieldss)]
                     [(field-num    ...) (map length fieldss)]
                     [((k ...)      ...) (map list->ks fieldss)]
                     [((variant* ...) ...)
                      (map (lambda (n fields) (map (lambda (_) n) fields))
                           variant-names fieldss)])
         (define t-tmp (datum->syntax-object #'type (syntax-e (car (generate-temporaries #'(type?))))))
         (define v-tmps (generate-temporaries #'(variant ...)))
         (with-syntax ([%type? t-tmp]
                       [(%variant ...) v-tmps]
                       ;; go over preds, and change `type?' to `%type?' to
                       ;; allow recursive types
                       [((pred*? ...) ...)
                        (map (lambda (preds)
                               (map (lambda (pred)
                                      (if (and (identifier? pred)
                                               (module-identifier=?
                                                pred #'type?))
                                        t-tmp pred))
                                    preds))
                             predss)]
                       [top-level-hack
                        (if (eq? 'top-level context)
                          (with-syntax ([%type? t-tmp])
                            (syntax/loc stx (define type? %type?)))
                          #'(mz:begin))])
           (quasisyntax/loc stx
             (mz:begin
              #,(syntax-property
                 (syntax/loc stx 
                   (mz:begin
                    (define %type?
                      (let ([type? (lambda (x) (or (variant? x) ...))]) type?))
                    top-level-hack
                    (define-values (%variant variant? variant-field ...)
                      (let-values ([(s:variant make-variant variant? get set)
                                    (make-struct-type 'variant #f field-num 0 #f
                                                      datatype-printer-props
                                                      (make-inspector))])
                        (define (variant field ...)
                          (make-variant
                           field
                           #;(if (pred*? field)
                                 field
                                 (error 'variant* "bad value for field `~s': ~e"
                                        'field field))
                             ...))
                        (values variant variant?
                                (make-struct-field-accessor get k 'field) ...)))
                    ...
                    (define-syntaxes (type? type
                                            variant ...)
                      (let ([*t (make-*t #'type #'type? #f)])
                        (let-values ([(variant ...)
                                      (values (make-stxinfo #'%variant
                                                            (make-*v #'variant #'variant? *t
                                                                     (list #'field ...)
                                                                     (list #'pred? ...)
                                                                     (list #'variant-field
                                                                           ...)))
                                              ...)])
                          (set-*t-*vs! *t (list (stxinfo-info variant) ...))
                          (values (make-stxinfo #'%type? *t) (make-stxinfo #f *t) variant ...))))))
                 'typechecker:ignore #t)
              (#%app void (quote-syntax (define-type-internal type %type? [variant %variant (field pred?) ...] ...))))))))]))

(define-syntax (cases stx)
  (define (val stx)
    (let ([v (and (identifier? stx) (syntax-local-value stx (lambda () #f)))])
      (and (stxinfo? v) (stxinfo-info v))))
  (define (get-*p pattern)
    (syntax-case pattern (else)
      [(variant sub ...)
       (let loop ([p pattern])
         (syntax-case p ()
           [(variant sub ...)
            (let* ([subs    (syntax->list #'(sub ...))]
                   [sub-len (length subs)]
                   [val     (val #'variant)])
              (if (*v? val)
                (begin
                  (unless (= sub-len (length (*v-fields val)))
                    (raise-syntax-error
                     #f (format "incorrect number of subpatterns; expecting ~s"
                                (length (*v-fields val)))
                     stx p))
                  (make-*p p val (*v-*t val) (map loop subs)))
                (raise-syntax-error
                 #f "pattern must use variant names" stx p)))]
           [id (identifier? #'id) #'id]
           [_ (raise-syntax-error #f "syntax error in clause" stx p)]))]
      [else #t]
      [_ (raise-syntax-error #f "syntax error in clause" stx pattern)]))
  ;; used for reporting missing cases
  (define (needed->string needed)
    (define (loop n)
      (cond [(*v? n)
             (format "(~a ...)" (symbol->string (syntax-e (*v-variant n))))]
            [(pair? n)
             (let ([variant (symbol->string (syntax-e (*v-variant (car n))))])
               (mapconcat
                (lambda (args)
                  (if args
                    (format "(~a ~a)" variant (mapconcat loop args " "))
                    "..."))
                (cdr n)
                ", "))]
            [(not n) "?"] ; #f stands for a plain predicate (primitive type)
            [else (error 'needed->string "internal error! ~s" n)]))
    (mapconcat loop needed ", "))
  (define (mappend f . ls) (apply append (apply map f ls)))
  (define (filter pred? l)
    (let loop ([l l] [r '()])
      (if (null? l)
        (reverse! r)
        (loop (cdr l) (if (pred? (car l)) (cons (car l) r) r)))))
  (define (mapconcat f l sep) ; like the Emacs function
    (if (null? l)
      ""
      (apply string-append (cdr (mappend (lambda (x) (list sep (f x))) l)))))
  ;; cartesian product of a list of option lists
  (define (x-prod ls)
    (if (null? ls)
      '(())
      (mappend (lambda (r) (map (lambda (x) (cons x r)) (car ls)))
               (x-prod (cdr ls)))))
  ;; Explodes a given variant so it is ready for subtracting the given
  ;; patterns.  The result is a variant descriptor, which is a list with a
  ;; variant info struct (*v) as a first element, and then there is a list of
  ;; cases that require covering.  Each such case has one variant descriptor
  ;; per argument for our variant.  Other than the top-level result, nested
  ;; descriptors can be unexpanded (when not required by the pattern) -- in
  ;; that case they will be just the *v struct.
  (define (explode-variant variant *ps)
    (unless (and (*v? variant) (andmap *p? *ps)) (error "internal error!"))
    ;; remove duplicate and irrelevant patterns
    (set! *ps (let ([v (cond [(*v? variant) variant]
                             [(pair? variant) (car variant)]
                             [else (error "internal error!")])])
                (filter (lambda (*p) (eq? (*p-*v *p) v))
                        (remove-duplicate-*p/*v *ps))))
    (if (null? *ps)
      variant
      (cons
       variant
       (remove-duplicate-*p/*v
        (x-prod
         (map (lambda (v ps)
                (let* ([ps (remove-duplicate-*p/*v ps)]
                       [*ps (filter *p? ps)] ; no identifiers for sub calls
                       [type (val v)])
                  (cond
                   ;; got a type => recurse
                   [type
                    (let ([tvs (*t-*vs type)]
                          [pvs (map (lambda (p)
                                      (cond [(*p? p) (*p-*v p)]
                                            [(identifier? p) p]
                                            [else (error "internal error!")]))
                                    ps)])
                      (if (andmap (lambda (pv)
                                    (or (identifier? pv) (memq pv tvs)))
                                  pvs)
                        (mappend
                         (lambda (pv)
                           (if (identifier? pv)
                             '(#f)
                             (mappend (lambda (tv)
                                        (if (eq? tv pv)
                                          (let ([expl (explode-variant
                                                       pv *ps)])
                                            (map (lambda (ex)
                                                   (cons (car expl) ex))
                                                 (cdr expl)))
                                          (list tv)))
                                      tvs)))
                         pvs)
                        (error "internal error!")))]
                   ;; type=#f => plain predicate => allow only ids
                   [(andmap identifier? ps) '(#f)]
                   [(ormap (lambda (p) (and (*p? p) p)) ps) =>
                    (lambda (p)
                      (raise-syntax-error #f
                        (format "cannot use a pattern for `~a' slot"
                                (syntax-object->datum v))
                        stx (*p-pattern p)))]
                   [else (error "internal error!")])))
              (*v-preds variant) (apply map list (map *p-subs *ps))))))))
  ;; removes all duplicate patterns (ignores identifier names) or variants
  ;; (possibly nested variant infos); for variants, drop plain (unexploded)
  ;; variants if there are exploded versions of this variant
  (define (remove-duplicate-*p/*v l)
    (define (same-*p/*v? x y) ; also true if exploded y covers unexploded x
      (cond [(identifier? x) (identifier? y)]
            [(*p? x) (and (*p? y)
                          (eq? (*p-*v x) (*p-*v y))
                          (andmap same-*p/*v? (*p-subs x) (*p-subs y)))]
            [(*v? x) (or (eq? x y) (and (pair? y) (eq? x (car y))))]
            [(list? x) (and (list? y)
                            (= (length x) (length y))
                            (andmap same-*p/*v? x y))]))
    (let loop ([l l] [r '()])
      (if (null? l)
        (reverse! r)
        (let ([x (car l)] [l (cdr l)])
          (loop l (if (or (ormap (lambda (y) (same-*p/*v? x y)) r)
                          ;; if trying variants, then don't add unexploded ones
                          ;; if there is an exploded down the list
                          (and (pair? x)
                               (*v? ((if (pair? (car x)) caar car) x))
                               (ormap (lambda (y) (same-*p/*v? x y)) l)))
                    r (cons x r)))))))
  ;; checks if the variant (*v or nested) is covered by sub
  (define (covered? v sub)
    (cond [(identifier? sub) #t] ; identifiers match all
          [(not (*p? sub)) (error "internal error!")]
          ;; nested variant info -- recurse if matching
          [(pair? v) (and (eq? (*p-*v sub) (car v))
                          (all-covered? (cdr v) (*p-subs sub)))]
          [(not (*v? v)) (error "internal error!")]
          ;; cannot have a match here: v is not nested => should be exploded
          [(eq? v (*p-*v sub)) (error "internal error!")]
          [else #f])) ; mismatch
  (define (all-covered? vs subs)
    (andmap covered? vs subs))
  ;; removes the cases of an exploded variant that are covered by the pattern
  (define (variant-subtract variant *p)
    (let ([pv (*p-*v *p)])
      (if (pair? variant)
        (if (eq? (car variant) pv)
          (cons (car variant)
                (let ([subs (*p-subs *p)])
                  (filter (lambda (v) (not (all-covered? v subs)))
                          (cdr variant))))
          ;; the pattern does not use this variant
          variant)
        ;; unexploded variant -- cannot be the same as the pattern's
        (if (eq? pv variant)
          (error "internal error!")
          variant))))
  (define (variants-subtract variants *p)
    (map (lambda (v) (variant-subtract v *p)) variants))
  ;; Main entry point for exhaustiveness check.  This is done by first
  ;; exploding all options according to the specified patterns, and then
  ;; removing cases that are covered by the pattern.  The two stages could
  ;; probably be combined into one scan, but it is pretty complex and fragile
  ;; as it is.
  (define (check-coverage *t *ps get-else)
    (let loop ([needed (let ([ps (filter *p? *ps)]) ; no else patterns
                         (map (lambda (v) (explode-variant v ps))
                              (*t-*vs *t)))]
               [*ps *ps])
      ;; `needed' is a list of exploded variant infos (see above)
      (cond
       [(pair? *ps)
        (if (eq? #t (car *ps))
          ;; `else': all done except when needed is empty
          (when (null? needed)
            (raise-syntax-error
             #f "redundant `else', pattern cases are exhaustive"
             stx (get-else)))
          ;; otherwise: needed := needed - pattern
          (let ([new (variants-subtract needed (car *ps))])
            (when (equal? new needed)
              ;; no change means that the pattern was redundant
              (raise-syntax-error
               #f "unreachable case due to redundant pattern"
               stx (*p-pattern (car *ps))))
            ;; remove (exploded) variants that have no cases left
            (loop (filter (lambda (x) (or (*v? x) (pair? (cdr x)))) new)
                  (cdr *ps))))]
       [(pair? needed) ; pair: leftover requirements, otherwise: all fine
        (raise-syntax-error
         #f (format "missing cases for the following variants: ~a"
                    (needed->string needed))
         stx)])))
  (define (check-*p-subtypes *t *p . more)
    ;; check only patterns, not identifiers or `else' clause
    (when (*p? *p)
      (unless (eq? *t (*p-*t *p))
        (raise-syntax-error
         #f (if (null? more) ; only toplevel cases
              (format
               "conflicting `~s' variant (expecting more `~s' variants)"
               (syntax-object->datum (*t-type (*p-*t *p)))
               (syntax-object->datum (*t-type *t)))
              (apply format
                     "expecting a variant of `~s' for `~s' field of `~s'"
                     (syntax-object->datum (*t-type *t))
                     (map syntax-object->datum more)))
         stx (*p-pattern *p)))
      (let ([*v (*p-*v *p)])
        (for-each
         (lambda (*p pred field)
           (let ([*t (val pred)])
             (when *t (check-*p-subtypes *t *p field (*v-variant *v)))))
         (*p-subs *p) (*v-preds *v) (*v-fields *v)))))
  (define (check-*ps-types patterns *ps)
    ;; check at least one clause
    (when (null? *ps) (raise-syntax-error #f "no clauses" stx))
    ;; check at most a single `else'
    (let ([else? (memq #t *ps)])
      (when (and else? (pair? (cdr else?)))
        (raise-syntax-error
         #f "`else' clause must be last" stx
         (list-ref patterns (- (length patterns) (length else?))))))
    ;; check at least one non-`else' clause
    (when (eq? #t (car *ps))
      (raise-syntax-error #f "must have non-else clauses" stx))
    (let ([*t (*p-*t (car *ps))])
      ;; check that variants are used with compatible types
      (for-each (lambda (*p) (check-*p-subtypes *t *p)) *ps)
      ;; find inexhaustive and unreachable patterns
      (check-coverage *t *ps
        ;; used if there is an error with the `else' case
        (lambda () (list-ref patterns (sub1 (length patterns)))))))
  (define (check-*p-bindings *p)
    (cond [(check-duplicate-identifier
            (let loop ([*p *p])
              (cond [(eq? #t *p) '()] ; `else'
                    [(identifier? *p) (list *p)]
                    [(*p? *p) (mappend loop (*p-subs *p))]
                    [else (error "internal error!")])))
           => (lambda (dup)
                (raise-syntax-error
                 #f "duplicate pattern identifier" stx dup))]))
  (define (convert val info body)
    (cond
     [(*p? info)
      (let ([*v (*p-*v info)])
        (with-syntax
            ([val val]
             [variant? (*v-variant? *v)]
             [body (let loop ([subs (*p-subs info)]
                              [tmps (generate-temporaries (*v-fields *v))]
                              [gets (*v-getters *v)])
                     (if (null? subs)
                       body
                       (with-syntax
                           ([tmp (car tmps)] [get (car gets)] [val val])
                         #`(let ([tmp (get val)])
                             #,(convert
                                #'tmp (car subs)
                                (loop (cdr subs) (cdr tmps) (cdr gets)))))))])
          #'(if (variant? val) body *failure*)))]
     [(identifier? info)
      (with-syntax ([val val] [id info] [body body])
        #'(let ([id val]) body))]
     [else (error "internal error!")]))
  (define (convert-clauses val infos0 bodies)
    ;; types are fine; at least one variant, at most a one last else
    (let loop ([infos infos0] [bodies bodies])
      (cond [(null? infos)
             #`(error 'cases "unmatched value in cases of ~s: ~e"
                      '#,(*t-type (*p-*t (car infos0))) #,val)]
            [(eq? #t (car infos)) (car bodies)] ; `else' case
            [else (with-syntax ([rest  (loop (cdr infos) (cdr bodies))]
                                [first (convert val (car infos) (car bodies))])
                    #'(let ([r first]) (if (eq? *failure* r) rest r)))])))
  (syntax-case stx ()
    [(_ expr [pattern body0 body ...] ...)
     (let ([bad-clause
            (lambda ()
              (raise-syntax-error
               #f "expecting an expression, got a clause" stx #'expr))])
       (syntax-case #'expr (else)
         [(else . _1) (bad-clause)]
         [[(variant . _1) . _2] (*v? (val #'variant)) (bad-clause)]
         [_else 'ok]))
     (let* ([patterns (syntax->list #'(pattern ...))]
            [*ps    (map get-*p patterns)]
            [bodies (syntax->list #'((mz:begin body0 body ... ) ...))]
            [val    #'val]
            [*t     (and (pair? *ps) (*p? (car *ps)) (*p-*t (car *ps)))])
       (check-*ps-types patterns *ps)
       (for-each check-*p-bindings *ps)
       (with-syntax ([type  (*t-type  *t)]
                     [type? (*t-type? *t)]
                     [body  (convert-clauses val *ps bodies)]
                     [val   val])
         (quasisyntax/loc stx
           (let ([val expr] [*failure* '#,(gensym 'failure)])
             (unless (type? val)
               (error 'cases "expecting `~s', got ~e" 'type val))
             body))))]))

)


#| ============================================================================

;; code for testing things out:

;; AE abstract syntax trees
(define-type AE
  [Num (n number?)]
  [Mxd (l AE?) (r number?)]
  [Add (lhs AE?) (rhs AE?)]
  [Sub (lhs AE?) (rhs AE?)]
  [Foo (x AE1?)])
(define-type AE1
  [Num1 (n number?)]
  [Num2 (n number?)])
;; eval : AE -> number
;; consumes an AE and computes the corresponding number
(define (Eval expr)
  (cases expr
    [(Num n)   n]
    [(Mxd (Num l) r) (+ l r)]
    [(Mxd l r) (+ (Eval l) r)] ; necessary
    [(Add (Num n) r) (+ n (Eval r))]
    [(Add l r) (+ (Eval l) (Eval r))] ; good (only partially covered)
    ;[(Add (Num n) r) (+ (Eval l) (Eval r))] ; already covered
    [(Sub (Num n) r) (- n (Eval r))]
    [(Sub (Add l r) rr) (- (+ (Eval l) (Eval r)) (Eval rr))]
    ;[(Sub (Sub l r) r) -1] ; bad bindings
    [(Sub (Sub l r) rr) (- (Eval l) (Eval r) (Eval rr))]
    [(Sub l r) (- (Eval l) (Eval r))]
    [(Foo (Num1 n)) n]
    [(Foo (Num2 n)) n] ; necessary
    ;[else 123] ; use if necessaries removed or to try redundant else error
    ))
;; tests:
(equal? 3 (Eval (Num 3)))
(equal? 7 (Eval (Add (Num 3) (Num 4))))
(equal? 6 (Eval (Add (Sub (Num 3) (Num 4)) (Num 7))))
(equal? 6 (Eval (Add (Num 2) (Foo (Num1 4)))))

(define-type BOOLEAN [T (n integer?)] [F])
(define-type PRODUCT [Pair (x BOOLEAN?) (y BOOLEAN?)])
(define-type PRODUCT* [Pair* (x PRODUCT?) (y PRODUCT?)])
(equal? 4 (cases (Pair (F) (F))
            [(Pair (T n1) (F)   ) 1]
            [(Pair (T n1) (T n2)) 2]
            [(Pair (F)    (T n2)) 3]
            [(Pair (F)    (F)   ) 4]))
(equal? 1 (cases (Pair* (Pair (T 1) (F)) (Pair (T 2) (F)))
            [(Pair* (Pair (T n1) (F)   ) (Pair (T n3) (F)   ))  1]
            [(Pair* (Pair (T n1) (T n2)) (Pair (T n3) (F)   ))  2]
            [(Pair* (Pair (F)    (T n2)) (Pair (T n3) (F)   ))  3]
            [(Pair* (Pair (F)    (F)   ) (Pair (T n3) (F)   ))  4]
            [(Pair* (Pair (T n1) (F)   ) (Pair (T n3) (T n4)))  5]
            [(Pair* (Pair (T n1) (T n2)) (Pair (T n3) (T n4)))  6]
            [(Pair* (Pair (F)    (T n2)) (Pair (T n3) (T n4)))  7]
            [(Pair* (Pair (F)    (F)   ) (Pair (T n3) (T n4)))  8]
            [(Pair* (Pair (T n1) (F)   ) (Pair (F)    (T n4)))  9]
            [(Pair* (Pair (T n1) (T n2)) (Pair (F)    (T n4))) 10]
            [(Pair* (Pair (F)    (T n2)) (Pair (F)    (T n4))) 11]
            [(Pair* (Pair (F)    (F)   ) (Pair (F)    (T n4))) 12]
            [(Pair* (Pair (T n1) (F)   ) (Pair (F)    (F)   )) 13]
            [(Pair* (Pair (T n1) (T n2)) (Pair (F)    (F)   )) 14]
            [(Pair* (Pair (F)    (T n2)) (Pair (F)    (F)   )) 15]
            [(Pair* (Pair (F)    (F)   ) (Pair (F)    (F)   )) 16]))

;; Matthew's test (should fail)
;; (define-type Foo [a (x integer?)])
;; (cases (a (a 10)) [(a (a x)) x])

|#
