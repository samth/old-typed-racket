(module type-effect-convenience mzscheme
  
  (require "type-rep.ss" "effect-rep.ss"
           (lib "plt-match.ss")
           "type-comparison.ss"
           "type-effect-printer.ss"
           "union.ss"
           (lib "list.ss")
           (prefix 1: (lib "list.ss" "srfi" "1")))
  
  (provide (all-defined))
    
  
  (define (-vet id) (make-Var-True-Effect id))
  (define (-vef id) (make-Var-False-Effect id))
  
  (define -rem make-Remove-Effect)
  (define -rest make-Restrict-Effect)
  
  (define (var->type-eff eff)
    (match eff
      [(Var-True-Effect: v) (make-Remove-Effect (make-Value #f) v)]
      [(Var-False-Effect: v) (make-Restrict-Effect (make-Value #f) v)]
      [_ eff]))
  
  (define ((add-var v) eff)
    (match eff
      [(Latent-Var-True-Effect:) (-vet v)]
      [(Latent-Var-False-Effect:) (-vef v)]
      [(Latent-Restrict-Effect: t) (make-Restrict-Effect t v)]
      [(Latent-Remove-Effect: t) (make-Remove-Effect t v)]
      [(True-Effect:) eff]
      [(False-Effect:) eff]
      [_ (error 'internal-tc-error "can't add var to effect ~a" eff)]))
  
  (define-syntax ->
    (syntax-rules (:)
      [(_ dom ... rng)
       (->* (list dom ...) rng)]
      [(_ dom ... rng : eff1 eff2)
       (->* (list dom ...) : eff1 eff2)]))
  (define-syntax ->*
    (syntax-rules (:)
      [(_ dom rng)       
       (make-Function (list (make-arr* dom rng)))]
      [(_ dom rst rng)
       (make-Function (list (make-arr* dom rng rst)))]
      [(_ dom rng : eff1 eff2)
       (make-Function (list (make-arr* dom rng #f eff1 eff2)))]
      [(_ dom rst rng : eff1 eff2)
       (make-Function (list (make-arr* dom rng rst eff1 eff2)))]))
  (define-syntax cl->
    (syntax-rules (:)
      [(_ [(dom ...) rng] ...)
       (make-Function (list (make-arr* (list dom ...) rng) ...))]
      [(_ [(dom ...) rng : eff1 eff2] ...)
       (make-Function (list (make-arr* (list dom ...) rng #f eff1 eff2) ...))]
      [(_ [(dom ...) rng rst : eff1 eff2] ...)
       (make-Function (list (make-arr* (list dom ...) rng rst eff1 eff2) ...))]))
  (define (cl->* . args)
    (define (funty-arities f)
      (match f
        [(Function: as) as]))
    (make-Function (map car (map funty-arities args))))
  
  (define make-arr*
    (case-lambda [(dom rng) (make-arr* dom rng #f (list) (list))]
                 [(dom rng rest) (make-arr dom rng rest (list) (list))]
                 [(dom rng rest eff1 eff2) (make-arr dom rng rest eff1 eff2)]))
  
  (define (make-promise-ty t)
    (make-Struct (string->uninterned-symbol "Promise") #f (list t) #f))
  
  (define N (make-Base 'Number))
  (define B (make-Base 'Boolean))
  (define Sym (make-Base 'Symbol))
  (define -Void (make-Base 'Void))
  (define -Bytes (make-Base 'Bytes))
  (define -Regexp (make-Base 'Regexp))
  (define -PRegexp (make-Base 'PRegexp))
  (define -Byte-Regexp (make-Base 'Byte-Regexp))
  (define -Byte-PRegexp (make-Base 'Byte-PRegexp))
  (define -String (make-Base 'String))
  (define -Keyword (make-Base 'Keyword))
  (define -Char (make-Base 'Char))
  (define -Syntax (make-Base 'Syntax))
  (define -Prompt-Tag (make-Base 'Prompt-Tag))
  (define -Cont-Mark-Set (make-Base 'Continuation-Mark-Set))
  (define -Path (make-Base 'Path))
  (define -Namespace (make-Base 'Namespace))
  (define -Output-Port (make-Base 'Output-Port))
  (define -Input-Port (make-Base 'Input-Port))  
  
  (define -HT make-Hashtable)
  (define -Promise make-promise-ty)

  (define Univ (make-Univ))
  
  (define-syntax -v 
    (syntax-rules ()
      [(_ x) (make-F 'x)]))
  
  (define-syntax -poly
    (syntax-rules ()
      [(_ (vars ...) ty)
       (let ([vars (-v vars)] ...)
         (make-Poly (list 'vars ...) ty))]))
  
  (define-syntax -mu
    (syntax-rules ()
      [(_ var ty)
       (let ([var (-v var)])
         (make-Mu 'var ty))]))
  
  
  (define -values make-Values)
  
  ;; produce the appropriate type of a list of types
  ;; that is - if there is exactly one type, just produce it, otherwise produce a values-ty
  ;; list[type] -> type
  (define (list->values-ty l)
    (if (= 1 (length l)) (car l) (-values l)))
  
  (define-syntax *Un
    (syntax-rules ()
      [(_ . args) (make-Union (list . args))]))

  
  (define -pair make-Pair)
  (define -base make-Base)
  
  (define -struct make-Struct)
  (define -val make-Value)
      
  (define (make-Listof elem) (-mu list-rec (*Un (-val null) (-pair elem list-rec))))
  (define -Listof (-poly (list-elem) (make-Listof list-elem)))
  
  (define -lst make-Listof)
  (define -Sexp (-mu x (*Un Sym N B -String (-val null) (-pair x x))))
  (define -Port (*Un -Input-Port -Output-Port))
  
  (define (-lst* . args) (if (null? args)
                             (-val null)
                             (-pair (car args) (apply -lst* (cdr args)))))
  

  #;(define NE (-mu x (Un N (make-Listof x))))
  (define -NE (-mu x (*Un N (-pair x (-pair Sym (-pair x (-val null)))))))
  
  (define (Un/eff . args)
    (apply Un (map tc-result-t args)))

  (define -Param make-Param)
  
  (define make-pred-ty
    (case-lambda 
      [(in out t)
       (->* in out : (list (make-Latent-Restrict-Effect t)) (list (make-Latent-Remove-Effect t)))]
      [(t) (make-pred-ty (list Univ) B t)]))

  (define -Pathlike (*Un -Path -String))
  (define -Pathlike* (*Un (-val 'up) (-val 'same) -Path -String))
  (define -Pattern (*Un -String -Bytes -Regexp -Byte-Regexp -PRegexp -Byte-PRegexp))
  (define -Byte N)
  
  ;; DO NOT USE if t contains #f
  (define (-opt t) (*Un (-val #f) t))
  
  (define-syntax (make-env stx)
    (syntax-case stx ()
      [(_ e ...)
       #`(list
          #,@(map (lambda (e)
                    (syntax-case e ()
                      [(nm ty)
                       (identifier? #'nm)
                       #`(list  #'nm ty)]
                      [(e ty)
                       #'(list (eval 'e (make-namespace)) ty)]))
                  (syntax->list #'(e ...))))]))

  
  )