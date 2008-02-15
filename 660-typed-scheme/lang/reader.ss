(module reader scheme/base
  (require (prefix-in r: "../typed-reader.ss"))
  
  (define (*read in)
    (wrap in r:read))
  
  (define (*read-syntax src in)
    (wrap in (lambda (in)
               (r:read-syntax src in))))
  
  (define (wrap port read)
    (let ([body
           (let loop ([a null])
             (let ([v (read port)])
               (if (eof-object? v)
                   (reverse a)
                   (loop (cons v a)))))])
      (let* ([p-name (object-name port)]
             [name (if (path? p-name)
                       (let-values ([(base name dir?) (split-path p-name)])
                         (string->symbol (path->string (path-replace-suffix name #""))))
                       'page)]
             [id 'doc])
        `(module ,name typed-scheme/lang/main (#%module-begin . ,body)))))
  
  (provide (rename-out [*read read] [*read-syntax read-syntax])))