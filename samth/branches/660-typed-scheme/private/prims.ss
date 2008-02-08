#lang scheme/base

#|

This file defines three sorts of primitives. All of them are provided into any module using the typed scheme language.

1. function definitions available to the user, extending the mzscheme builtins
   examples include: cars, fst, atom?, build-vector
   all of these ought to go away eventually, some by fixing requiring, some by strengthing the type system

2. macros for defining type annotated code. 
   this includes: lambda:, define:, etc
   potentially, these macros should be replacements for the mzscheme ones in the user program
   however, it's nice to be able to use both when the sugar is more limited
   for example: (define ((f x) y) (+ x y))

3. macros for defining 'magic' code
   examples: define-typed-struct, require/typed
   these expand into (ignored) mzscheme code, and declarations that a typechecker understands
   in order to protect the declarations, they are wrapped in `#%app void' so that local-expand of the module body
   will not expand them on the first pass of the macro expander (when the stop list is ignored)

|#


(provide (all-defined-out))

(require (for-syntax 
          scheme/base
          "type-rep.ss"
          (lib "match.ss")
          "parse-type.ss"
          (lib "struct.ss" "syntax")
          (lib "stx.ss" "syntax")
          "utils.ss"   
          "tc-utils.ss"
          "type-name-env.ss"
          "type-contract.ss"))

(require "require-contract.ss"
         "internal-forms.ss"
         "planet-requires.ss"           
         (lib "etc.ss")
         (except-in (lib "contract.ss") ->)
         (only-in (lib "contract.ss") [-> c->])
         (lib "struct.ss")
         "base-env.ss")







(define-for-syntax (ignore stx) (syntax-property stx 'typechecker:ignore #t))



(define-syntax (require/typed stx)
  
  (syntax-case* stx (rename) (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(_ nm ty lib)
     (identifier? #'nm)
     ;; b/c we parse this here, we can't use types defined in this module
     (with-syntax ([cnt (type->contract (parse-type/id #'nm #'ty)
                                        (lambda () (tc-error/stx #'ty "Cannot convert type ~a to a contract" #'ty)))])
       (quasisyntax/loc stx (begin (#%app void (quote-syntax (require/typed-internal nm ty)))
                                   #,(syntax-property #'(require/contract nm cnt lib)
                                                      'typechecker:ignore #t))))]
    [(_ (rename internal-nm nm) ty lib)
     (identifier? #'nm)
     ;; b/c we parse this here, we can't use types defined in this module
     (with-syntax ([cnt (type->contract (parse-type/id #'nm #'ty))])
       (quasisyntax/loc stx (begin (#%app void (quote-syntax (require/typed-internal nm ty)))
                                   #,(syntax-property #'(require/contract nm cnt lib)
                                                      'typechecker:ignore #t))))]
    [(_ lib [nm ty] ...)
     #'(begin (require/typed nm ty lib) ...)]))      

(define-syntax (require/opaque-type stx)
  (syntax-case stx ()
    [(_ ty pred lib)
     (and (identifier? #'ty) (identifier? #'pred))
     (with-syntax ([pred-cnt #'(any/c . c-> . boolean?)])
       (begin
         (register-type-name #'ty (make-Opaque #'pred (syntax-local-certifier)))
         (quasisyntax/loc stx
           (begin (#%app void (quote-syntax (require/typed-internal pred (Any -> Boolean : (Opaque pred)))))
                  (define-type-alias ty (Opaque pred))
                  #,(syntax-property #'(require/contract pred pred-cnt lib)
                                     'typechecker:ignore #t)))))]))

(define-for-syntax (types-of-formals stx)
  (syntax-case stx (:)
    [([var : ty] ...) (quasisyntax/loc stx (ty ...))]
    [([var : ty] ... . [rest : rest-ty]) (syntax/loc stx (ty ... rest-ty ..))])
  )


(define-syntax (plambda: stx)
  (syntax-case stx ()
    [(plambda: (tvars ...) formals . body)
     (syntax-property #'(lambda: formals . body)
                      'typechecker:plambda
                      #'(tvars ...))]))

(define-syntax (pcase-lambda: stx)
  (syntax-case stx ()
    [(pcase-lambda: (tvars ...) cl ...)
     (syntax-property #'(case-lambda: cl ...)
                      'typechecker:plambda
                      #'(tvars ...))]))

(define-syntax (pdefine: stx)
  (syntax-case stx (:)
    [(pdefine: tvars (nm . formals) : ret-ty . body)
     (with-syntax* ([(tys ...) (types-of-formals #'formals)]
                    [type (syntax/loc #'ret-ty (All tvars (tys ... -> ret-ty)))])
                   (syntax/loc stx
                     (define: nm : type
                       (plambda: tvars formals . body))))]))


#;(define-for-syntax (relocate stx loc)
    (with-syntax ([x stx])
      (syntax/loc loc x)))

(define-syntax (ann stx)
  (syntax-case stx (:)
    [(_ arg : ty)
     #;#'(let: ([id : ty arg]) id)
     (syntax-property #'arg 'type-ascription #'ty)]))

(define-syntax (: stx)
  (syntax-case stx ()
    [(_ id ty)
     (identifier? #'id)
     #'(#%plain-app void (quote-syntax (:-internal id ty)))]))

(define-syntax (inst stx)
  (syntax-case stx (:)
    [(_ arg : tys ...)
     (syntax-property #'arg 'type-inst #'(tys ...))]))


(define-syntax (-define-values stx)
  (syntax-case stx ()
    [(_ (vars ...) body)
     (eq? (syntax-local-context) 'module)
     (with-syntax* ([(fresh-vars ...) (generate-temporaries #'(vars ...))]
                    [(props ...) (map (lambda (v) (syntax-property v 'type-label))
                                      (syntax->list #'(vars ...)))]
                    [(fresh ...) (map (lambda (id orig-id)
                                        (let ([prop-stx (syntax-property #'dummy 'defined-type-label
                                                                         (syntax-property orig-id 'type-label))])
                                          (datum->syntax id (syntax-e id) orig-id prop-stx)))
                                      (syntax->list #'(fresh-vars ...))
                                      (syntax->list #'(vars ...)))])
                   (with-syntax ([def-fresh (syntax/loc stx (define-values (fresh ...) body))])
                     #'(begin
                         def-fresh
                         (define-syntax vars
                           (make-set!-transformer
                            (lambda (s)
                              (define (fresh-id n)
                                (syntax-property
                                 #;(relocate #'fresh n)
                                 (datum->syntax-object #'fresh (syntax-e #'fresh) n #f)
                                 'defined-type-label
                                 (datum->syntax-object n (syntax-object->datum #'props) #'props)))
                              (syntax-case s (set!)
                                [(set! nm v) (quasisyntax/loc s (set! #,(fresh-id #'nm) v))]
                                [(nm . args) (quasisyntax/loc s (#,(fresh-id #'nm) . args))]
                                [n (identifier? #'n) (fresh-id #'n)])))) ...)))]
    [(_ . rest) 
     (syntax/loc stx (define-values . rest))]))


(define-syntax (define: stx)
  (syntax-case stx (:)
    [(define: (nm . formals) : ret-ty body ...)
     (with-syntax* ([(tys ...) (types-of-formals #'formals)]
                    [arrty (syntax/loc stx (tys ... -> ret-ty))])
                   (syntax/loc stx
                     (define: nm : arrty
                       (lambda: formals body ...))))]
    [(define: nm : ty body)
     (identifier? #'nm)
     (with-syntax ([new-nm (syntax-property #'nm 'type-label #'ty)])
       (syntax/loc stx (define new-nm body)))]
    [(define: (vars ...) (f args ...) : ret body ...)
     #'(pdefine: (vars ...) (f args ...) : ret body ...)]))

(define-syntax (-define stx)
  (syntax-case stx ()
    [(_ (f . args) . b)
     (eq? (syntax-local-context) 'module)       
     (syntax/loc stx (-define f (lambda args . b)))]
    [(_ v b)       
     (eq? (syntax-local-context) 'module)
     (syntax/loc stx (-define-values (v) b))]
    [(_ . rest)
     (syntax/loc stx (define . rest))]))

;; helper function for annoating the bound names
(define-for-syntax (annotate-names stx)
  (define (label-one var ty)
    (syntax-property var 'type-label ty))
  (define (label vars tys)
    (map label-one
         (syntax->list vars)
         (syntax->list tys)))
  (syntax-case stx (:)
    [[var : ty] (label-one #'var #'ty)]
    [([var : ty] ...)
     (label #'(var ...) #'(ty ...))]
    [([var : ty] ... . [rest : rest-ty])
     (append (label #'(var ...) #'(ty ...)) (label-one #'rest #'rest-ty))]))       

(define-syntax-rule (λ: . args) (lambda: . args))

(define-syntax (lambda: stx)
  (syntax-case stx (:)
    [(lambda: formals . body)
     (with-syntax ([labeled-formals (annotate-names #'formals)])
       (syntax/loc stx (lambda labeled-formals . body)))]))

(define-syntax (case-lambda: stx)
  (syntax-case stx (:)
    [(case-lambda: [formals . body] ...)
     (with-syntax ([(lab-formals ...) (map annotate-names (syntax->list #'(formals ...)))])
       (syntax/loc stx (case-lambda [lab-formals . body] ...)))]))

(define-syntaxes (let-internal: let*: letrec:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-case stx (:)
                  [(_ ([nm : ty . exprs] ...) . body)
                   (with-syntax* ([(vars ...) (annotate-names #'([nm : ty] ...))]
                                  [bindings (map (lambda (v e loc)
                                                   (quasisyntax/loc loc [#,v . #,e]))
                                                 (syntax->list #'(vars ...))
                                                 (syntax->list #'(exprs ...))
                                                 (syntax->list (syntax-case stx () [(_ bs . body) #'bs])))])
                                 (quasisyntax/loc stx (#,form bindings . body)))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntax (let: stx)
  (syntax-case stx (:)
    [(let: nm : ret-ty ([arg : ty val] ...) . body)
     (identifier? #'nm)
     (syntax/loc stx ((letrec: ([nm : (ty ... -> ret-ty) (lambda: ([arg : ty] ...) . body)]) nm) val ...))]
    [(let: . rest)
     (syntax/loc stx (let-internal: . rest))]))

(define-syntax (define-type-alias stx)
  (syntax-case stx ()
    [(_ tname rest) 
     (identifier? #'tname)
     (begin
       #`(begin
           #,(ignore #'(define-syntax tname (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))))
           (#%app void (quote-syntax (define-type-alias-internal tname rest)))))]
    [(_ (tname . args) rest)
     (andmap identifier? (syntax->list #'args))
     #'(define-type-alias tname (All args rest))]))

(define-syntax (define-typed-struct/exec stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) [proc : proc-ty])
     (with-syntax* 
      ([proc* (syntax-property #'(ann proc : proc-ty) 'typechecker:with-type #t)]
       [d-s (syntax-property (syntax/loc stx (define-struct/properties nm (fld ...)
                                               ([prop:procedure proc*])))
                             'typechecker:ignore-some #t)]
       [dtsi (syntax/loc stx (#%app void 
                                    (quote-syntax (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty))))])
      #'(begin d-s dtsi))]))

(define-syntax (with-handlers: stx)
  (syntax-case stx ()
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...) (map (lambda (s) (syntax-property #`(ann #,s : (Any -> Any)) 'typechecker:with-type #t))
                                      (syntax->list #'(pred? ...)))]
                   [(action* ...)
                    (map (lambda (s) (syntax-property s 'typechecker:exn-handler #t)) (syntax->list #'(action ...)))]
                   [body* (syntax-property #'(begin . body) 'typechecker:exn-body #t)])
       (syntax-property #'(with-handlers ([pred?* action*] ...) body*)
                        'typechecker:with-handlers
                        #t))]))

(define-syntax (define-typed-struct stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) . opts)
     (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fld ...) . opts))
                                         'typechecker:ignore #t)]
                   [dtsi (syntax/loc stx (#%app void (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...)))))])
       #'(begin d-s dtsi))]
    [(_ (vars ...) nm ([fld : ty] ...) . opts)
     (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fld ...) . opts))
                                         'typechecker:ignore #t)]
                   [dtsi (syntax/loc stx (#%app void (quote-syntax (define-typed-struct-internal (vars ...) nm ([fld : ty] ...)))))])
       #'(begin d-s dtsi))]))

(define-syntax (require-typed-struct stx)
  (syntax-case stx (:)
    [(_ nm ([fld : ty] ...) lib)
     (with-syntax* ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                    [oty #'(Opaque pred)])
                   #'(begin
                       (require/opaque-type nm pred lib)
                       (require/typed maker (ty ... -> oty) lib)
                       (require/typed sel (oty -> ty) lib) ...))]))


;; the old version, copied from eli
(define-syntax (--define-type stx)
  (syntax-case stx (:)
    [(define-datatype nm [variant (fld ty) ...] ...)
     (with-syntax* (;; create new names for the internal structs
                    [(variant* ...) (generate-temporaries #'(variant ...))]
                    ;; figure out what the names for the "external" structs are
                    [((struct-info maker pred sel ...) ...) (map (lambda (name flds) (build-struct-names name flds #f #t name))
                                                                 (syntax->list #'(variant ...))
                                                                 (map syntax->list (syntax->list #'((fld ...) ...))))]
                    ;; names for the real internal structs
                    [((struct-info* maker* pred* sel* ...) ...) (map (lambda (name flds) (build-struct-names name flds #f #t name))
                                                                     (syntax->list #'(variant* ...))
                                                                     (map syntax->list (syntax->list #'((fld ...) ...))))]
                    ;; generate the code to create all the structs and the extra bindings
                    [d-s (syntax/loc stx
                           (begin (define-struct nm ())
                                  (define-struct (variant* nm) (fld ...)) ...
                                  (define variant maker*) ...
                                  (define pred pred*) ...
                                  (begin (define sel sel*) ...) ...))]
                    [ddi (quasisyntax/loc stx (#%app void (quote-syntax (define-type-internal nm [variant variant* (fld ty) ...] ...))))])
                   #`(begin
                       #,(ignore #'d-s)
                       ddi))]))

;; new, simpler version
(define-syntax (-define-type stx)
  (define (ignore stx) (syntax-property stx 'typechecker:ignore #t))
  (syntax-case stx (:)
    [(define-datatype nm [variant (fld ty) ...] ...)
     (with-syntax* (;; make the name for the top predicate
                    [(_ _m top-pred . other) (build-struct-names #'nm null #f #f #'nm)]
                    ;; figure out what the names for the structs are
                    [((struct-info maker pred sel ...) ...) (map (lambda (name flds) (build-struct-names name flds #f #t name))
                                                                 (syntax->list #'(variant ...))
                                                                 (map syntax->list (syntax->list #'((fld ...) ...))))]
                    ;; create the maker names
                    [(maker* ...) #'(maker ...)]
                    ;; generate the code to create all the structs and the extra bindings
                    [d-s (syntax/loc stx
                           (begin 
                             (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx)))
                             (define-struct variant (fld ...)) ...
                             (define (top-pred arg) (or (pred arg) ...))))]
                    [ddi (quasisyntax/loc stx (#%app void (quote-syntax (define-type-internal nm top-pred [variant maker* (fld ty) ...] ...))))])
                   #`(begin
                       #,(ignore #'d-s)
                       ddi))]))

(define-syntax (do: stx)
  (syntax-case stx (:)
    [(_ : ty ((var : tys init . step) ...) (e0 e1 ...) c ...)
     (with-syntax ([(step ...)
                    (map (lambda (v s)
                           (syntax-case s ()
                             [() v]
                             [(e) #'e]
                             [_ (raise-syntax-error 
                                 #f
                                 "bad variable syntax"
                                 stx)]))
                         (syntax->list #'(var ...))
                         (syntax->list #'(step ...)))])
       (syntax-case #'(e1 ...) ()
         [() (syntax/loc
                 stx
               (let: doloop : ty ([var : tys init] ...)
                     (if (not e0)
                         (begin c ... (doloop step ...)))))]
         [(e1 e2 ...)
          (syntax/loc
              stx
            (let: doloop : ty ([var : tys init] ...)
                  (if e0
                      (begin e1 e2 ...)
                      (begin c ... (doloop step ...)))))]))]))

;; this macro allows smaller transformations of cond to 
;; allow the typechecker to understand the code
(define-syntax (cond* stx)
  (syntax-case stx (else and or)
    ;; the appropriate transformation greatly increases code size
    #;[(cond* [(and e0 es ...) . b] cls ...)
       (if e0 (cond* [(and es ...) . b] cls ...) #f)]
    [(cond* [pred acc var body ...] cl ...)
     (identifier? #'var)
     #'(let ([var acc])
         (if (pred var)
             (begin body ...)
             (cond* cl ...)))]
    [(cond* [else e ...]) #'(begin e ...)]
    [(cond* cl cls ...)
     #'(cond cl [else (cond* cls ...)])]
    [(cond*) #'(cond)]))


