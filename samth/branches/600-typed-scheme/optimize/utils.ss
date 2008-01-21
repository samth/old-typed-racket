(module utils mzscheme
  (require (lib "string.ss") (lib "plt-match.ss"))  

  ;; hack to make it work with syntax coverage
  (define-syntax (sourced-match stx)
    (syntax-case stx ()
      [(_ val clause ...)
       (datum->syntax-object
        stx
        (syntax-e (local-expand (syntax/loc stx (match val clause ...))
                                'expression '()))
        stx
        stx)]))
  (define-syntax (define-predicate-matcher stx)
    (syntax-case stx ()
      [(_ pred ty)
       (identifier? #'pred)
       (let* ([name (symbol->string (syntax-e #'pred))]
              [id (lambda (sfx)
                    (let ([sym (string->symbol (string-append name sfx))])
                      (datum->syntax-object stx sym stx stx)))])
         (with-syntax ([pred: (id ":")] [pred? (id "?")])
           #'(define-match-expander pred:
               (lambda (s)
                 (syntax-case s (pred:) 
                   [(pred: pat) (syntax-property #'(? pred? pat) 'type-ascription 'ty)])))))]))
  (define-predicate-matcher number Number)
  (define-predicate-matcher integer Number)
  (define-predicate-matcher symbol Symbol)
  (define-predicate-matcher string String)
  (define-predicate-matcher boolean Boolean)
  (provide (rename sourced-match match)
           number: integer: symbol: string: boolean:)

  ;; ========== a few saner bindings ==========================================
  ;; some sane bindings, similar to the teaching languages

  (provide *if)
  (define-syntax *if
    (syntax-rules () [(*if cond then else) (if cond then else)]))

  (provide *list? *cons)
  (define (*list? x)
    (or (null? x) (pair? x)))
  (define (*cons x l)
    (if (*list? l) (cons x l) (raise-type-error 'cons "list" 1 x l)))

  ;; ========== string "parser" ===============================================

  ;; build a regexp that matches restricted character expressions, can use only
  ;; {}s for lists, and limited strings that use '...' (normal mzscheme escapes
  ;; like \n, and '' for a single ')
  (define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
  ;; this would make it awkward for students to use \" for strings
  ;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
  (define good-string "[^\"\\']*(?:''[^\"\\']*)*")
  (define expr-re
    (regexp (string-append "^"
                           good-char"*"
                           "(?:'"good-string"'"good-char"*)*"
                           "$")))
  (define string-re
    (regexp (string-append "'("good-string")'")))

  (provide string->sexpr)
  (define (string->sexpr str)
    (unless (string? str)
      (raise-type-error 'string->sexpr "string" 0 str))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                   (regexp-replace*
                    "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
      (if (= 1 (length sexprs))
        (car sexprs)
        (error 'string->sexpr "bad syntax (multiple expressions)"))))

  ;; ========== pattern-matching transformations ==============================

  (define (transform-fail:syntax thunk from to)
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (raise (make-exn:fail:syntax
                               (string->immutable-string
                                (regexp-replace (format "^~a:" from)
                                                (exn-message e)
                                                (format "~a:" to)))
                               (exn-continuation-marks e)
                               (exn:fail:syntax-exprs e))))])
      (thunk)))

  (provide make-transformer)
  (define (make-transformer literals from+to-list)
    (define transformer
      (transform-fail:syntax
       (lambda () (eval `(syntax-rules ,literals ,@from+to-list)))
       'syntax
       'transform))
    (lambda (expr)
      (syntax-object->datum (transformer (datum->syntax-object #f expr)))))

  ;; ========== tests =========================================================

  (provide test test-mode test-postprocess)

  (define test-mode        (make-parameter 'report))
  (define test-inspector   (make-parameter (current-inspector)))
  (define test-postprocess (make-parameter values))

  (define (install-test-inspector)
    (test-inspector (current-inspector))
    (current-inspector (make-inspector))
    (print-struct #t))

  (define-syntax (test stx)
    (define (ignore stx) (syntax-property stx 'typechecker:ignore #t))
    (unless (memq (syntax-local-context) '(top-level module module-begin))
      (raise-syntax-error
       #f "can be used only as a top-level expression"
       stx))
    (syntax-case* stx (=> <= =error> <error=)
      (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
      [(test expr)
       (ignore (syntax/loc stx (test-1 expr 'expr)))]
      [(test expr1 => expr2)
       (ignore (syntax/loc stx (test-2 expr1 'expr1 expr2 'expr2)))]
      [(test expr2 <= expr1)
       (ignore (syntax/loc stx (test-2 expr1 'expr1 expr2 'expr2)))]
      [(test expr =error> msg-re)
       (ignore (syntax/loc stx
                 (test-e (lambda () ((test-postprocess) expr)) 'expr msg-re)))]
      [(test msg-re <error= expr)
       (ignore (syntax/loc stx
                 (test-e (lambda () ((test-postprocess) expr)) 'expr msg-re)))]))
  (define-struct (exn:test exn) ())
  (define (test-error fmt . args)
    (parameterize ([current-inspector (test-inspector)]
                   [print-struct #t])
      (let ([msg (string-append "Test failure: " (apply format fmt args))])
        (case (test-mode)
          [(report) (fprintf (current-error-port) "~a\n" msg)]
          [(verbose error)
           (raise (make-exn:test (string->immutable-string msg)
                                 (current-continuation-marks)))]
          [else (error 'test "bad test-mode: ~e" (test-mode))]))))
  (define (test-ok label)
    (parameterize ([current-inspector (test-inspector)]
                   [print-struct #t])
      (when (eq? 'verbose (test-mode))
        (fprintf (current-error-port) "Test passed: ~e\n" label))
      (void)))
  (define (test-1 val expr)
    (if ((test-postprocess) val)
      (test-ok expr)
      (test-error "~e failed" expr)))
  (define (test-2 val1 expr1 val2 expr2)
    (parameterize ([current-inspector (test-inspector)])
      (let ([val1 ((test-postprocess) val1)]
            [val2 ((test-postprocess) val2)])
        (if (equal? val1 val2)
          (test-ok expr1)
          (test-error "~e failed: got ~e, but expected ~e" expr1 val1 val2)))))
  (define (test-e thunk expr msg-re)
    (let ([r (with-handlers ([exn:fail? (lambda (e) e)]) (thunk))])
      ;; assume normal code does not return an exception
      (cond
        [(not (exn? r)) (test-error "~e did not signal an error" expr)]
        [(not (regexp-match-positions (simple-glob->regexp msg-re)
                                      (exn-message r)))
         (test-error "mismatched error message in ~e (expecting \"~a\"): ~a"
                     expr msg-re (exn-message r))]
        [else (test-ok expr)])))

  (define glob-item-re
    (regexp (string-append "(?:"
                           "[\\]." ; escaped item
                           "|"
                           "[*?]"  ; wildcards -- the only 1-character match
                           ")")))
  (define (simple-glob->regexp glob)
    (let loop ([i 0] [ps (regexp-match-positions* glob-item-re glob)] [r '()])
      (if (null? ps)
        (regexp (apply string-append
                       (reverse (cons (regexp-quote (substring glob i)) r))))
        (loop (cdar ps) (cdr ps)
              ;; length=1 is only for `*' or `?'
              (cons (if (= 1 (- (cdar ps) (caar ps)))
                      (if (equal? #\* (string-ref glob (caar ps))) ".*" ".")
                      (substring glob (caar ps) (cdar ps)))
                    (if (= i (caar ps))
                      r (cons (regexp-quote (substring glob i (caar ps)))
                              r)))))))

  (install-test-inspector)

  ;; ========== additional type stuff =========================================

  (provide any? list-of sexp-of box-of union-of intersection-of false? true?)
  (define (any? x) #t)
  (define (list-of pred?)
    (define (list-of-proc l) (and (*list? l) (andmap pred? l)))
    list-of-proc)
  (define (sexp-of pred?) ; restricted to proper lists
    (define (sexp-of-proc s)
      (let loop ([s s]) (or (pred? s) (and (*list? s) (andmap loop s)))))
    sexp-of-proc)
  (define (box-of pred?)
    (define (box-of-proc x) (and (box? x) (pred? (unbox x))))
    box-of-proc)
  (define (union-of . preds)
    (define (union-of-proc x) (ormap (lambda (p?) (p? x)) preds))
    union-of-proc)
  (define (intersection-of . preds)
    (define (intersection-of-proc x) (andmap (lambda (p?) (p? x)) preds))
    intersection-of-proc)
  (define (false? x) (not x))
  (define (true? x) (not (not x)))

  )
