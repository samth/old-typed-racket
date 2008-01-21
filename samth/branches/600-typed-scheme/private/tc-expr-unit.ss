#lang scheme/unit


(require (prefix-in 1: srfi/1)
         syntax/kerncase
         syntax/struct
         syntax/stx
         mzlib/etc
         mzlib/plt-match
         "type-contract.ss"
         "signatures.ss"
         "tc-structs.ss"
         "type-utils.ss"
         "utils.ss" ;; doesn't need tests
         "type-rep.ss" ;; doesn't need tests
         "unify.ss" ;; needs tests
         "infer.ss"
         "type-effect-convenience.ss" ;; maybe needs tests
         "union.ss"
         "subtype.ss" ;; has tests
         "internal-forms.ss" ;; doesn't need tests
         "planet-requires.ss" ;; doesn't need tests
         "type-env.ss" ;; maybe needs tests
         "parse-type.ss" ;; has tests
         "tc-utils.ss" ;; doesn't need tests
         "type-environments.ss" ;; doesn't need tests
         "lexical-env.ss" ;; maybe needs tests
         "type-annotation.ss" ;; has tests
         "type-name-env.ss" ;; maybe needs tests
         "init-envs.ss"
         "effect-rep.ss"
         "mutated-vars.ss"
         (lib "plt-match.ss"))

(require (for-template scheme/base))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ check-subforms^)
(export tc-expr^)


;; return the type of a literal value
;; scheme-value -> type
(define (tc-literal v-stx)
  ;; find the meet of the types of a list of expressions
  ;; list[syntax] -> type
  (define (types-of-literals es)
    (apply Un (map tc-literal es)))
  (define v (syntax-e v-stx))
  (cond
    [(number? v) N]
    [(char? v) -Char]
    [(boolean? v) (-val v)]
    [(null? v) (-val null)]
    [(symbol? v) (-val v)]
    [(string? v) -String]
    [(keyword? v) -Keyword]
    [(bytes? v) -Bytes]
    [(list? v) (make-Listof (types-of-literals v))]
    [(vector? v) (make-Vector (types-of-literals (vector->list v)))]
    [(pregexp? v) -PRegexp]
    [(byte-pregexp? v) -Byte-PRegexp]
    [(byte-regexp? v) -Byte-Regexp]
    [(regexp? v) -Regexp]
    [else (begin (printf "checking literal : ~a~n" v) Univ)]))

;; typecheck an identifier
;; the identifier has variable effect
;; tc-id : identifier -> tc-result
(define (tc-id id)
  (let* ([ty (lookup-type/lexical id)]
         [inst (syntax-property id 'type-inst)])
    (when (and inst
               (not (Poly? ty)))
      (tc-error "Cannot instantiate non-polymorphic type ~a" ty))
    (when (and inst
               (not (= (length (syntax->list inst)) (Poly-n ty))))
      (tc-error "Wrong number of type arguments to polymorphic type ~a:~nexpected: ~a~ngot: ~a"
                ty (Poly-n ty) (length (syntax->list inst))))
    (let ([ty* (if inst
                   (begin
                     (printf/log "Type Instantiation: ~a~n" (syntax-e id))
                     (instantiate-poly ty (map parse-type (syntax->list inst))))
                   ty)])
      (ret ty* (list (make-Var-True-Effect id)) (list (make-Var-False-Effect id))))))

;; typecheck an expression, but throw away the effect
;; tc-expr/t : Expr -> Type
(define (tc-expr/t e) (match (tc-expr e)
                        [(tc-result: t) t]))

(define (check-below tr1 expected)
  (match (list tr1 expected)      
    [(list (tc-result: t1 te1 ee1) t2)
     (unless (subtype t1 t2)
       (tc-error "Expected ~a, but got ~a" t2 t1))
     (ret expected)]
    [(list t1 t2)
     (unless (subtype t1 t2)
       (tc-error "Expected ~a, but got ~a" t2 t1))
     (ret expected)]
    [_ (error "bad arguments to check-below")]))

(define (tc-expr/check form expected)
  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a~n" (syntax-object->datum form))
    ;; the argument must be syntax
    (unless (syntax? form) 
      (int-err "bad form input to tc-expr: ~a" form))
    (let (;; a local version of ret that does the checking
          [ret 
           (lambda args
             (define te (apply ret args))
             (check-below te expected)
             (ret expected))])
      (kernel-syntax-case* form #f 
        (letrec-syntaxes+values) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
        [stx
         (syntax-property form 'typechecker:with-handlers)
         (check-subforms/with-handlers/check form expected)]
        [stx 
         (syntax-property form 'typechecker:ignore-some)
         (let ([ty (check-subforms/ignore form)])
           (unless ty
             (tc-error "internal error: ignore-some"))
           (check-below ty expected))]
        ;; data
        [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
        [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
        [(quote val)  (ret (tc-literal #'val))]
        ;; syntax
        [(quote-syntax datum) (ret -Syntax)]
        ;; mutation!
        [(set! id val)
         (match-let* ([(tc-result: id-t) (tc-id #'id)]
                      [(tc-result: val-t) (tc-expr #'val)])
           (unless (subtype val-t id-t)
             (tc-error "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
           (ret -Void))]
        ;; top-level variable reference - occurs at top level
        [(#%top . id) (check-below (tc-id #'id) expected)]
        ;; weird
        [(#%variable-reference . _)
         (tc-error "#%variable-reference is not supported by Typed Scheme")]
        ;; identifiers
        [x (identifier? #'x) (check-below (tc-id #'x) expected)]
        ;; w-c-m
        [(with-continuation-mark e1 e2 e3)
         (begin (tc-expr/check #'e1 Univ)
                (tc-expr/check #'e2 Univ)
                (tc-expr/check #'e3 expected))]  
        ;; application        
        [(#%plain-app . _) (tc/app/check form expected)]
        ;; #%expression
        [(#%expression e) (tc-expr/check #'e expected)]
        ;; syntax
        ;; for now, we ignore the rhs of macros
        [(letrec-syntaxes+values stxs vals . body)
         (tc-expr/check (syntax/loc form (letrec-values vals . body)) expected)]
        ;; begin
        [(begin e . es) (tc-exprs/check (syntax->list #'(e . es)) expected)]
        [(begin0 e . es)
         (begin (tc-exprs/check (syntax->list #'es) Univ)
                (tc-expr/check #'e expected))]          
        ;; if
        [(if tst body) (tc/if-onearm/check #'tst #'body expected)]
        [(if tst thn els) (tc/if-twoarm/check #'tst #'thn #'els expected)]
        ;; lambda
        [(#%plain-lambda formals . body)
         (tc/lambda/check form #'(formals) #'(body) expected)]        
        [(case-lambda [formals . body] ...)
         (tc/lambda/check form #'(formals ...) #'(body ...) expected)]      
        ;; let
        [(let-values ([(name ...) expr] ...) . body)
         (tc/let-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
        [(letrec-values ([(name ...) expr] ...) . body)
         (tc/letrec-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
        ;; other
        [_ (tc-error "cannot typecheck unknown form : ~a~n" (syntax->datum form))]
        ))))

;; type check form in the current type environment
;; if there is a type error in form, or if it has the wrong annotation, error
;; otherwise, produce the type of form
;; syntax[expr] -> type
(define (tc-expr form)
  ;; do the actual typechecking of form
  ;; internal-tc-expr : syntax -> Type    
  (define (internal-tc-expr form)
    (kernel-syntax-case* form #f 
      (letrec-syntaxes+values #%datum #%app lambda) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
      ;; 
      [stx
       (syntax-property form 'typechecker:with-handlers)
       (let ([ty (check-subforms/with-handlers form)])
         (unless ty
           (tc-error "internal error: with-handlers"))
         ty)]
      [stx 
       (syntax-property form 'typechecker:ignore-some)
       (let ([ty (check-subforms/ignore form)])
         (unless ty
           (tc-error "internal error: ignore-some"))
         ty)]
      
      ;; data
      [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
      [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
      
      [(quote val)  (ret (tc-literal #'val))]
      ;; syntax
      [(quote-syntax datum) (ret -Syntax)]
      ;; w-c-m
      [(with-continuation-mark e1 e2 e3)
       (begin (tc-expr/check #'e1 Univ)
              (tc-expr/check #'e2 Univ)
              (tc-expr #'e3))]
      ;; lambda
      [(#%plain-lambda formals . body)
       (tc/lambda form #'(formals) #'(body))]        
      [(case-lambda [formals . body] ...)
       (tc/lambda form #'(formals ...) #'(body ...))]      
      ;; let
      [(let-values ([(name ...) expr] ...) . body)
       (tc/let-values #'((name ...) ...) #'(expr ...) #'body form)]
      [(letrec-values ([(name ...) expr] ...) . body)
       (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body form)]        
      ;; mutation!
      [(set! id val)
       (match-let* ([(tc-result: id-t) (tc-id #'id)]
                    [(tc-result: val-t) (tc-expr #'val)])
         (unless (subtype val-t id-t)
           (tc-error "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
         (ret -Void))]        
      ;; top-level variable reference - occurs at top level
      [(#%top . id) (tc-id #'id)]
      ;; #%expression
      [(#%expression e) (tc-expr #'e)]
      ;; weird
      [(#%variable-reference . _)
       (tc-error "do not use #%variable-reference")]
      ;; identifiers
      [x (identifier? #'x) (tc-id #'x)]                 
      ;; application        
      [(#%plain-app . _) (tc/app form)]
      ;; if
      [(if tst body) (tc/if-twoarm #'tst #'body #'(#%app void))]
      [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els)]                          
      
      ;; syntax
      ;; for now, we ignore the rhs of macros
      [(letrec-syntaxes+values stxs vals . body)
       (tc-expr (syntax/loc form (letrec-values vals . body)))]
      
      ;; begin
      [(begin e . es) (tc-exprs (syntax->list #'(e . es)))]
      [(begin0 e . es)
       (begin (tc-exprs (syntax->list #'es))
              (tc-expr #'e))]
      ;; other
      [_ (tc-error "cannot typecheck unknown form : ~a~n" (syntax->datum form))]))
  
  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a~n" (syntax->datum form))
    ;; the argument must be syntax
    (unless (syntax? form) 
      (int-err "bad form input to tc-expr: ~a" form))
    ;; typecheck form
    (cond [(type-annotation form) => (lambda (ann) 
                                       ;(printf "annotated~n")
                                       (tc-expr/check form ann))]
          [else (internal-tc-expr form)])))

;; type-check a list of exprs, producing the type of the last one.
;; if the list is empty, the type is Void.
;; list[syntax[expr]] -> tc-result
(define (tc-exprs exprs)
  (cond [(null? exprs) (ret -Void)]
        [(null? (cdr exprs)) (tc-expr (car exprs))]
        [else (tc-expr/check (car exprs) Univ)
              (tc-exprs (cdr exprs))]))

(define (tc-exprs/check exprs expected)
  (cond [(null? exprs) (check-below (ret -Void) expected)]
        [(null? (cdr exprs)) (tc-expr/check (car exprs) expected)]
        [else (tc-expr/check (car exprs) Univ)
              (tc-exprs/check (cdr exprs) expected)]))