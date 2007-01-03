(define (f x)
  (define-struct foo (bar))
  (values (lambda (y) (make-foo (+ x y))) foo-bar))

(define-values (foo1 bar1) (f 10))

(define-values (foo2 bar2) (f 20))

(bar2 (foo1 33))