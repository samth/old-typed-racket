(module base-env mzscheme
  
(require-for-template (only (lib "list.ss") foldl)
                        mzscheme
                        (prefix 660: (file "../optimize/csu660.ss"))
                        (file "../optimize/utils.ss")
                        #%paramz
                        #%more-scheme
                        #%qq-and-or
                        (lib "match-error.ss" "mzlib" "private" "match")
                        )

  
  (require
   "extra-procs.ss"
   (all-except "type-rep.ss" make-arr)
   (only (lib "list.ss") foldl)
   "type-effect-convenience.ss"
   (rename "type-effect-convenience.ss" make-arr make-arr*)
   "union.ss"
   (lib "match-error.ss" "mzlib" "private" "match")
   "tc-structs.ss")
  
  (require-for-syntax
   "init-envs.ss"
   (all-except "type-rep.ss" make-arr)
   (only (lib "list.ss") foldl)
   "type-effect-convenience.ss"
   (rename "type-effect-convenience.ss" make-arr make-arr*)
   "union.ss"
   (lib "match-error.ss" "mzlib" "private" "match")
   "tc-structs.ss")
  
  (define-for-syntax (initialize-others)
    (tc/builtin-struct #'date 
                       #f
                       (list #'second #'minute #'hour #'day #'month #'year #'week-day #'year-day #'dst? #'time-zone-offset)
                       (list N N N N N N N N B N)
                       null)
    (tc/builtin-struct #'exn
                       #f
                       (list #'message #'continuation-marks)
                       (list -String Univ)
                       null)
    (tc/builtin-struct #'exn:fail
                       #'exn
                       null                       
                       null
                       (list -String Univ))
    (tc/builtin-struct #'exn:fail:read
                       #'exn:fail
                       (list #'srclocs)                       
                       (list (-lst Univ))
                       (list -String Univ)))

     
  (provide-for-syntax initial-env initialize-others)
  
  (define-for-syntax initial-env
    (let ([make-lst make-Listof]
          [make-lst/elements -pair])
      (make-env
       
       (car (make-Poly (list 'a 'b) (cl-> [((-pair (-v a) (-v b))) (-v a)]
                                          [((make-lst (-v a))) (-v a)])))
       (cadr 
        (-poly (a b c)
               (cl->                
                [((-pair a (-pair b c))) b]
                [((-lst a)) a])))
       (caddr (-poly (a) (-> (-lst a) a)))
       (cadddr (-poly (a) (-> (-lst a) a)))
       (cdr (make-Poly (list 'a 'b) (cl-> [((-pair (-v a) (-v b))) (-v b)]
                                          [((make-lst (-v a))) (make-lst (-v a))])))
       (cddr (make-Poly (list 'a) (-> (make-lst (-v a)) (make-lst (-v a)))))
       (cdddr (make-Poly (list 'a) (-> (make-lst (-v a)) (make-lst (-v a)))))
       (cons (-poly (a b) 
                    (cl-> [(a (-lst a)) (-lst a)]
                          [(a b) (-pair a b)])))
       
       (null? (make-pred-ty (-val null)))
       (eof-object? (make-pred-ty (-val eof)))
       [null (-val null)]
       (number? (make-pred-ty N))
       (boolean? (make-pred-ty B))
       (add1 (-> N N))
       (sub1 (-> N N))
       (eq? (-> Univ Univ B))
       (eqv? (-> Univ Univ B))
       (equal? (-> Univ Univ B))
       (even? (-> N B))
       [assert (-poly (a) (-> (*Un a (-val #f)) a))]
       [gensym (cl-> [(Sym) Sym]
                     [() Sym])]
       [string-append (->* null -String -String)]
       [open-input-string (-> -String -Input-Port)]
       [open-output-file 
        (cl->
         [(-Pathlike) -Port]
         [(-Pathlike Sym) -Port])]
       [read (cl-> 
              [(-Port) -Sexp]
              [() -Sexp])]
       [ormap (-poly (a b) ((-> a b) (-lst a) . -> . b))]
       [andmap (-poly (a b) ((-> a b) (-lst a) . -> . b))]
       [newline (cl-> [() -Void]
                      [(-Port) -Void])]
       [not (-> Univ B)]
       [floor (-> N N)]
       [cons? (make-pred-ty (-pair Univ Univ))]
       [pair? (make-pred-ty (-pair Univ Univ)) #;(-poly (a b) (make-pred-ty (-pair a b)))]
       [empty? (make-pred-ty (-val null))]
       [empty (-val null)]
       [string? (make-pred-ty -String)]
       [symbol? (make-pred-ty Sym)]
       [list? (make-pred-ty (-lst Univ))]
       [list (-poly (a) (->* '() a (-lst a)))]
       [procedure? (make-pred-ty (->* '() (Un) Univ))]
       [map 
        (-poly (a b c d)
               (cl-> [((-> a b) (-lst a)) (-lst b)]
                     [((-> a b c) (-lst a) (-lst b)) (-lst c)]
                     [((-> a b c d) (-lst a) (-lst b) (-lst c)) (-lst d)]))]
       [for-each 
        (-poly (a b c)
               (cl-> [((-> a b) (-lst a)) -Void]
                     [((-> a b c) (-lst a) (-lst b)) -Void]))]
       [foldl (make-Poly '(a b) 
                         (-> (-> (-v a) (-v b) (-v b)) (-v b) (make-lst (-v a)) (-v b)))]
       
       [call-with-values (-poly (a b) (-> (-> a) (-> a b) b))]
       
       (error 
        (make-Function (list 
                     (make-arr null (Un))
                     (make-arr (list Sym -String) (Un) Univ)
                     (make-arr (list -String) (Un) Univ)
                     (make-arr (list Sym) (Un)))))
       
       [integer? (Univ . -> . B)]
       [namespace-variable-value 
        (cl-> 
         [(Sym) Univ]
         [(Sym B -Namespace (-> Univ)) Univ])]
        
       (match:error (Univ . -> . (Un)))
       (display 
        (cl->
         [(Univ) -Void]
         [(Univ -Port) -Void]))
       [void (->* '() Univ -Void)]
       [void? (make-pred-ty -Void)]
       [printf (->* (list -String) Univ -Void)]
       [fprintf (->* (list -Output-Port -String) Univ -Void)]
       [format (->* (list -String) Univ -String)]   
       (fst (make-Poly (list 'a 'b) (-> (make-lst/elements (-v a) (-v b)) (-v a))))
       (snd (make-Poly (list 'a 'b) (-> (make-lst/elements (-v a) (-v b)) (-v b))))
       
       (= (->* (list N N) N B))
       (>= (->* (list N N) N B))
       (< (->* (list N N) N B))
       (<= (->* (list N N) N B))
       [> (->* (list N) N B)]
       (zero? (N . -> . B))
       (* (->* '() N N))
       (/ (->* (list N) N N))
       (+ (->* '() N N))
       (- (->* (list N) N N))
       (max (->* (list N) N N))
       (min (->* (list N) N N))
       [values  (make-Poly '(a) (-> (-v a) (-v a)))]
       [vector-ref 
        (make-Poly (list 'a) ((make-Vector (-v a)) N . -> . (-v a)))]
       [build-vector (make-Poly (list 'a) (N (N . -> . (-v a)) . -> . (make-Vector (-v a))))]
       [reverse (make-Poly '(a) (-> (make-lst (-v a)) (make-lst (-v a))))]
       [append (-poly (a) (->* (list) (-lst a) (-lst a)))]
       [length (make-Poly '(a) (-> (make-lst (-v a)) N))]
       [memq (make-Poly (list 'a) (-> (-v a) (make-lst (-v a)) (-opt (make-lst (-v a)))))]
       [memv (make-Poly (list 'a) (-> (-v a) (make-lst (-v a)) (-opt (make-lst (-v a)))))]
       [member 
        (-poly (a) (a (-lst a) . -> . (-opt (-lst a))))]
       
       [string<? (->* (list -String -String) -String B)]
       [string>? (->* (list -String -String) -String B)]
       [string=? (->* (list -String -String) -String B)]
       [string<=? (->* (list -String -String) -String B)]
       [string>=? (->* (list -String -String) -String B)]

       [string-ci<? (->* (list -String -String) -String B)]
       [string-ci>? (->* (list -String -String) -String B)]
       [string-ci=? (->* (list -String -String) -String B)]
       [string-ci<=? (->* (list -String -String) -String B)]
       [string-ci>=? (->* (list -String -String) -String B)]
       
       [string-upcase (-> -String -String)]
       [string-downcase (-> -String -String)]
       [string-titlecase (-> -String -String)]
       [string-foldcase (-> -String -String)]
       
       [string-normalize-nfd (-> -String -String)]
       [string-normalize-nfkd (-> -String -String)]
       [string-normalize-nfc (-> -String -String)]
       [string-normalize-nfkc (-> -String -String)]
       
       [assq (-poly (a) (-> Univ (-lst (-pair Univ a)) a))]

       [build-path ((list -Pathlike*) -Pathlike* . ->* . -Path)]
       [string->number (-> -String (-opt N))]
       [with-input-from-file 
           (-poly (a) 
                  (cl->
                   [(-Pathlike  (-> a))  a]
                   [(-Pathlike (-> a) Sym) a]))]
       [with-output-to-file
           (-poly (a) 
                  (cl->
                   [(-Pathlike  (-> a))  a]
                   [(-Pathlike (-> a) Sym) a]))]
       
       [random (cl->
                [(N) N]
                [() N])]
       
       [assoc (-poly (a b) (a (-lst (-pair a b)) . -> . (-opt (-pair a b))))]
       
       [list-ref (-poly (a) ((-lst a) N . -> . a))]
       [positive? (-> N B)]
       [negative? (-> N B)]
       [odd? (-> N B)]
       [even? (-> N B)]
       
       [apply (-poly (a b) (((list) a . ->* . b) (-lst a) . -> . b))]
       
       [call/cc (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (*Un a b)))]
       [call/ec (-poly (a b) (((a . -> . (Un)) . -> . b) . -> . (*Un a b)))]
       
       [quotient (N N . -> . N)]
       [remainder (N N . -> . N)]
       [quotient/remainder (N N . -> . (-values (list N N)))]
       
       ;; parameter stuff
       
       [parameterization-key Sym]
       [extend-parameterization (-poly (a b) (-> Univ (-Param a b) a Univ))]
       [continuation-mark-set-first (-> (-opt -Cont-Mark-Set) Univ Univ)]
       [make-parameter (-poly (a b) (cl-> [(a) (-Param a a)]
                                         [(b (a . -> . b)) (-Param a b)]))]
       [current-directory (-Param -Pathlike -Path)]
       [current-namespace (-Param -Namespace -Namespace)]
       [print-struct (-Param B B)]
       [read-decimal-as-inexact (-Param B B)]
       [current-command-line-arguments (-Param (make-Vector -String) (make-Vector -String))]
       
       ;; regexp stuff
       [regexp-match 
        (cl->
         [((*Un -String -Regexp) -String) (-opt (-lst (-opt -String)))]
         [(-Pattern -String) (-opt (-lst (-opt (*Un -Bytes -String))))]
         [(-Pattern -String N) (-opt (-lst (-opt (*Un -Bytes -String))))]
         [(-Pattern -String N (-opt N)) (-opt (-lst (-opt (*Un -Bytes -String))))]
         [(-Pattern -String N (-opt N) (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
         [(-Pattern -String (-opt N) (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
         [(-Pattern -String (-opt -Output-Port)) (-lst (-opt (*Un -Bytes -String)))]
         [(-Pattern (*Un -Input-Port -Bytes)) (-opt (-lst (-opt -Bytes)))]
         [(-Pattern (*Un -Input-Port -Bytes) N) (-opt (-lst (-opt -Bytes)))]
         [(-Pattern (*Un -Input-Port -Bytes) N (-opt N)) (-opt (-lst (-opt -Bytes)))]
         [(-Pattern (*Un -Input-Port -Bytes) (-opt N)) (-opt (-lst (-opt -Bytes)))]
         [(-Pattern (*Un -Input-Port -Bytes) N (-opt N) (-opt -Output-Port)) (-lst (-opt -Bytes))])]
        
       
       [number->string (N . -> . -String)]
       
       [current-milliseconds (-> N)]
       [modulo (N N . -> . N)]
       
       ;; errors
       
       [raise-type-error
        (cl->
         [(Sym -String Univ) (Un)]
         [(Sym -String N (-lst Univ)) (Un)])]
       
       ;; this is a hack
       
       [match:error ((list) Univ . ->* . (Un))]
       
       [vector-set! (-poly (a) (-> (make-Vector a) N a -Void))]
       
       [vector->list (-poly (a) (-> (make-Vector a) (-lst a)))]
       [list->vector (-poly (a) (-> (-lst a) (make-Vector a)))]
       [exact? (N . -> . B)]
       [inexact? (N . -> . B)]
       [expt (N N . -> . N)]
       [vector (-poly (a) (->* (list) a (make-Vector a)))]
       [real? (Univ . -> . B)]
       [real-part (N . -> . N)]
       [imag-part (N . -> . N)]
       [magnitude (N . -> . N)]
       [angle (N . -> . N)]
       [numerator (N . -> . N)]
       [denominator (N . -> . N)]
       [exact->inexact (N . -> . N)]
       [inexact->exact (N . -> . N)]
       [make-string
        (cl->
         [(N) -String]
         [(N -Char) -String])]
       [arithmetic-shift (N N . -> . N)]
       [abs (N . -> . N)]
       [substring (cl-> [(-String N) -String]
                        [(-String N N) -String])]
       [string-length (-String . -> . N)]
       [string-set! (-String N -Char . -> . -Void)]
       [make-vector
        (-poly (a)
               (cl->
                [(N) (make-Vector N)]
                [(N a) (make-Vector a)]))]
       
       [file-exists? (-Pathlike . -> . B)]
       [string->symbol (-String . -> . Sym)]
       [symbol->string (Sym . -> . -String)]
       [vector-length (-poly (a) ((make-Vector a) . -> . N))]
       
       [call-with-input-file (-poly (a) 
                                     (cl->
                                      [(-String (-Port . -> . a))  a]
                                      [(-String (-Port . -> . a) Sym)  a]))]
         
       [call-with-output-file (-poly (a) 
                                     (cl->
                                      [(-String (-Port . -> . a))  a]
                                      [(-String (-Port . -> . a) Sym)  a]))]
       [current-output-port (-Param -Output-Port -Output-Port)]
       [current-error-port (-Param -Output-Port -Output-Port)]
       [current-input-port (-Param -Input-Port -Input-Port)]
       [round (N . -> . N)]
       [seconds->date (N . -> . (make-Struct 'date #f (list N N N N N N N N B N) #f))]
       [current-seconds (-> N)]
       [sqrt (-> N N)]
       [path->string (-> -Path -String)]       
       
       [link-exists? (-> -Pathlike B)]
       [directory-exists? (-> -Pathlike B)]
       [file-exists? (-> -Pathlike B)]
       [directory-list (cl-> [() (-lst -Path)]
                             [(-Path) (-lst -Path)])]
       
       [make-hash-table (let ([ht-opt (*Un (-val 'weak) (-val 'equal))])
                               (-poly (a b) 
                               (cl-> [() (-HT a b)]
                                     [(ht-opt) (-HT a b)]
                                     [(ht-opt ht-opt) (-HT a b)])))]
       
       [hash-table-put! (-poly (a b) ((-HT a b) a b . -> . -Void))]
       [hash-table-map (-poly (a b c) ((-HT a b) (a b . -> . c) . -> . (-lst c)))]
       [hash-table-get (-poly (a b c)
                              (cl->
                               (((-HT a b) a) b)
                               (((-HT a b) a (-> c)) (*Un b c))
                               (((-HT a b) a c) (*Un b c))))]
       #;[hash-table-index (-poly (a b) ((-HT a b) a b . -> . -Void))]
       
       [bytes (->* (list) N -Bytes)]
       [bytes-ref (-> -Bytes N N)]
       [bytes-append (->* (list -Bytes) -Bytes -Bytes)]
       [subbytes (cl->
                  [(-Bytes N) -Bytes]
                  [(-Bytes N N) -Bytes])]
       [bytes-length (-> -Bytes N)]
       [open-input-file (-> -Pathlike -Input-Port)]
       [close-input-port (-> -Input-Port -Void)]
       [close-output-port (-> -Output-Port -Void)]   
       [read-line (cl->
                   [() -String]
                   [(-Input-Port) -String]
                   [(-Input-Port Sym) -String])]
       [copy-file (-> -Pathlike -Pathlike -Void)]  
       [bytes->string/utf-8 (-> -Bytes -String)]
       ;; make-promise #;
       [(eval '(cadr (syntax->list (expand #'(delay 3)))) (make-namespace)) (-poly (a) (-> (-> a) (-Promise a)))]
       ;; qq-append #;
       [(eval '(cadr (syntax->list (expand #'`(,@'())))) (make-namespace)) (-poly (a b) 
                                                                 (cl->*
                                                                  (-> (-lst a) (-val '()) (-lst a))
                                                                  (-> (-lst a) (-lst b) (-lst (*Un a b)))))]
       [force (-poly (a) (-> (-Promise a) a))]
       [bytes<? (->* (list -Bytes) -Bytes B)]
       [regexp-replace* 
        (cl->*
         (-Pattern (*Un -Bytes -String) (*Un -Bytes -String) . -> . -Bytes)
         (-Pattern -String -String . -> . -String))]
       [peek-char
        (cl->*
         [-> -Char]
         [-Input-Port . -> . -Char]
         [-Input-Port N . -> . -Char]
         [N . -> . -Char])]
       [peek-byte
        (cl->*
         [-> -Byte]
         [-Input-Port . -> . -Byte]
         [-Input-Port N . -> . -Byte]
         [N . -> . -Byte])]
       [make-pipe
        (cl->*
         [-> (-values (list -Input-Port -Output-Port))]
         [N . -> . (-values (list -Input-Port -Output-Port))])]
       [open-output-bytes
        (cl->*
         [-> -Output-Port]
         [Univ . -> . -Output-Port])]
       [get-output-bytes
        (cl->*
         [-Output-Port . -> . -Bytes]
         [-Output-Port Univ . -> . -Bytes]
         [-Output-Port Univ N . -> . -Bytes]
         [-Output-Port Univ N N . -> . -Bytes]
         [-Output-Port N . -> . -Bytes]
         [-Output-Port N N . -> . -Bytes])]
       #;[exn:fail? (-> Univ B)]
       #;[exn:fail:read? (-> Univ B)]
       
       [write (-> -Sexp -Void)]
       [open-output-string (-> -Output-Port)]
       ;; FIXME - wrong
       [get-output-string (-> -Output-Port -String)]
       
       [make-directory (-> -Path -Void)]
       
       [hash-table-for-each (-poly (a b c)
                                   (-> (-HT a b) (-> a b c) -Void))]
       
       [delete-file (-> -Pathlike -Void)]
       [make-namespace (cl->* (-> -Namespace)
                              (-> (*Un (-val 'empty) (-val 'initial)) -Namespace))]
       [eval (-> -Sexp Univ)]
   
       [exit (-> (Un))]
       )))
  
  (begin-for-syntax (initialize-type-env initial-env)
                    (initialize-others))
   
 ;; the initial type name environment - just the base types
  (define-syntax (define-tname-env stx)
    (syntax-case stx ()
      [(_ var provider [nm ty] ...)
       #`(begin
           (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
           (provide nm) ...
           (define-syntax provider (lambda (stx) #'(begin (provide nm) ...)))
           (provide provider)
           (begin-for-syntax
             (initialize-type-name-env
              (list (list #'nm ty) ...))))]))
  
  (define-syntax (define-other-types stx)
    (syntax-case stx ()
      [(_ provider nm ...)
       (with-syntax ([(nms ...) (generate-temporaries #'(nm ...))])
         #'(begin (define-syntax nms (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
                  (provide nms) ...
                  (define-syntax provider (lambda (stx) #'(begin (provide (rename nms nm)) ...)))
                  (provide provider)))]))
  
  ;; the initial set of available type names
  (define-tname-env initial-type-names provide-tnames
    [Number N]
    [Void -Void]
    [Boolean B]
    [Symbol Sym]
    [String -String]
    [Any Univ]
    [Port -Port]
    [Path -Path]
    [num-exp -NE]
    [Regexp -Regexp]
    [PRegexp -PRegexp]
    [Char -Char]
    [Option (-poly (a) (-opt a))]
    [Sexp -Sexp]
    [List (-lst Univ)]
    [Listof -Listof]
    [Namespace -Namespace]
    [Input-Port -Input-Port]
    [Output-Port -Output-Port]
    [Bytes -Bytes]
    [EOF (-val eof)]
    [Keyword -Keyword]
    [HashTable (-poly (a b) (-HT a b))]
    [Promise (-poly (a) (-Promise a))]
    [Pair (-poly (a b) (-pair a b))]
    #;[Parameter -Param]
    ;; legacy abbreviations
    [top Univ]
    [str -String]
    [number N]
    [boolean B]
    [symbol Sym]
    [list-of -Listof]    
    )
  
  (define-other-types
    provide-extra-tnames
    -> U mu Un All Opaque Vectorof
    Parameter Tuple
    )

  )