(module internal-fail "../typed-scheme.ss"
  ;3
  (require "../private/internal-forms.ss")
  #;(define-typed-struct foo ([a : Number]))
  (#%expression (quote-syntax (define-typed-struct-internal foo ([a : Number]))))
  (make-foo 3))