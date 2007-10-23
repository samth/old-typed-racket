(module datatype-printer-props mzscheme
  (require (lib "pconvert-prop.ss"))
  (provide datatype-printer-props)
  ;; the printer is created here in a aseparate module, because datatype.ss is
  ;; re-used in a lazy-scheme context and in that case we still need the
  ;; printer to be eager (otherwise the write/display call generates a promise
  ;; and nothing else).
  (define (datatype-printer x port write?)
    ((if write? write display)
     (let ([l (vector->list (struct->vector x))])
       (cons (string->symbol
              (regexp-replace #rx"^struct:" (symbol->string (car l)) ""))
             (cdr l)))
     port))
  (define datatype-printer-props
    `(;; always show as constructor-style
      (,prop:custom-write . ,datatype-printer)
      ;; also when constructor-style is used
      (,prop:print-convert-constructor-name . variant))))
