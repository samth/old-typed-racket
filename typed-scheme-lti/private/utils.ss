(module utils mzscheme
  (provide with-syntax* syntax-map start-timing do-time reverse-begin define-simple-syntax printf/log
           with-logging-to-file log-file-name)
  
  (define-syntax (with-syntax* stx)
    (syntax-case stx ()
      [(_ (cl) body ...) #'(with-syntax (cl) body ...)]
      [(_ (cl cls ...) body ...)
       #'(with-syntax (cl) (with-syntax* (cls ...) body ...))]
      ))

  (define (syntax-map f stxl)
    (map f (syntax->list stxl)))
  
  (define-syntax reverse-begin
    (syntax-rules () [(_ h . forms) (begin0 (begin . forms) h)]))
  
  (define-syntax define-simple-syntax
    (syntax-rules ()
      [(dss (n . pattern) template)
       (define-syntax n (syntax-rules () [(n . pattern) template]))]))
  
  (define log-file (make-parameter #f))
  (define-for-syntax logging? #f)
  
  (require (lib "file.ss"))
  (define-syntax (printf/log stx)
    (if logging?         
        (syntax-case stx ()
          [(_ fmt . args) 
           #'(when (log-file)
               (apply fprintf (log-file) (string-append "~a: " fmt) 
                      (file-name-from-path (object-name (log-file)))
                      args))])
        #'(void)))
  
  (define (log-file-name src module-name)
    (if (path? src)
        (path-replace-suffix src ".log")
        (format "~a.log" module-name)))
  
  (define-syntax (with-logging-to-file stx)
    (syntax-case stx ()
      [(_ file . body)
       (if logging?
           #'(parameterize ([log-file (open-output-file file 'truncate/replace)])
               . body)
           #'(begin . body))]))

  
  (define-for-syntax timing? #f)
  
  (define last-time (make-parameter #f))
  (define-syntaxes (start-timing do-time)
    (if timing?        
        (values
         (syntax-rules ()
           [(_ msg)
            (begin
              (when (last-time)
                (error #f "Timing already started"))
              (last-time (current-process-milliseconds))
              (printf "Starting ~a at ~a~n" msg (last-time)))])
         (syntax-rules ()
           [(_ msg)
            (begin
              (unless (last-time)
                (start-timing msg))
              (let* ([t (current-process-milliseconds)]
                     [old (last-time)]
                     [diff (- t old)])
                (last-time t)
                (printf "Timing ~a at ~a@~a~n" msg diff t)))]))
        (values (lambda _ #'(void)) (lambda _ #'(void)))))
  
  
  (define (symbol-append . args)
    (string->symbol (apply string-append (map symbol->string args))))
    
  )