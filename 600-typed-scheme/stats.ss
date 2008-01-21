#! /bin/sh
#|
exec /usr/bin/env mzscheme -qu "$0" ${1+"$@"}
|#
;; a program for gathering statistics:

(module stats mzscheme
  
  (require (planet "io.ss" ("dherman" "io.plt" 1))
           (only (lib "13.ss" "srfi") string-delete)
           (only (lib "43.ss" "srfi") vector-for-each)
           (rename (lib "48.ss" "srfi") s:format format )
	   (lib "match.ss"))
  
  
  (define total-parens 0)
  (define total-lines 0)
  (define total-idty 0)
  (define total-nonidty 0)
  (read-accept-reader #t)
  
  (define (do-stats i fname)
    
    (define (ip) (open-input-file fname))
    
    (define lines (read-lines (ip)))
    (define line-count (length lines))
    
    (define sexps
      (let ([port (ip)])
        (let loop ([l '()])
          (let ([v (read port)])
            (if (eof-object? v) 
                l
                (loop (cons v l)))))))
    
    (define chars (apply + (map string-length lines)))
    (define nbchars (apply + (map (lambda (s) (string-length (string-delete char-whitespace? s))) lines)))
    
    
    (define (ann-count s)
      (let ([idty 0]
	    [nonidty 0])
	(define (inc s)
	  (match s
		 [(? symbol?) (set! idty (add1 idty))]
		 [()  (set! idty (add1 idty))]
		 #;[('Listof (? symbol?)) (set! idty (add1 idty))]
		 [('cons (? symbol?) (? symbol?)) (set! idty (add1 idty))]
		 [_
		  (set! nonidty (add1 nonidty))]))
	(let ann-count ([s s])
	  (match s
		 [((? symbol? id) ': ty) (inc ty)]
		 [((? symbol? id) ': ty v) (inc ty)]
		 [('let: _ ': ty . rest) (inc ty) #;(printf "doing let:~a~n" rest) (ann-count rest)]
		 [('ann _ ': ty) (inc ty)]
		 [('define: f ': ty . rest)
		  (inc ty)
		  (ann-count f)
		  (ann-count rest)]
		 [(a ...) (for-each ann-count a)]
		 [(a . b) (ann-count a) (ann-count b)]
		 [_ (void)]))
	(values idty nonidty)))

    (define (sexp-count s)
      #;(printf "sexp: ~a~n" s)
      (cond
        [(null? s) 1]
	[(list? s) (apply + (map sexp-count s))]
        [(pair? s) (+ 1 (sexp-count (car s)) (sexp-count (cdr s)))]
        [else 1]))
    
    (define-values (idty nonidty) (ann-count sexps))

    (define parens (apply + (map sexp-count sexps)))
    (define (/* a b) (/ (exact->inexact a) b))
    
    (printf "~a:~n" fname)
    
    (printf "Lines: ~a~nSexps: ~a~nParens: ~a~nChars: ~a~nNB Chars: ~a ~n" 
            line-count
            (length sexps)
            parens
            chars 
            nbchars)

    (printf "Id annotations: ~a~nNonid Annotations: ~a~n" idty nonidty)
    
    (set! total-idty (+ total-idty idty))
    (set! total-nonidty (+ total-nonidty nonidty))
    
    (set! total-parens (+ total-parens parens))
    (set! total-lines (+ total-lines line-count))
    
    #;
    (printf "Parens/Lines:~a~nChars/Lines:~a~nChars/Parens:~a~nNBChars/Lines:~a~nNBChars/Parens:~a~n"
            (/* parens line-count)
            (/* chars line-count)
            (/* chars parens)
            (/* nbchars line-count)
            (/* nbchars parens))
    #;
    (display 
     (s:format "Parens/Lines:~12,2F\nChars/Lines:~13,2F\nChars/Parens:~12,2F\nNBChars/Lines:~11,2F\nNBChars/Parens:~10,2F\n"
               (/* parens line-count)
               (/* chars line-count)
               (/* chars parens)
               (/* nbchars line-count)
               (/* nbchars parens)))
    (printf "===============================~n"))
      
  (vector-for-each do-stats (current-command-line-arguments))
  
  (printf "Total Parens: ~a~n" total-parens)
  (printf "Total Idty: ~a~n" total-idty)
  (printf "Total Nonidty: ~a~n" total-nonidty)
  (printf "Total Lines: ~a~n" total-lines)
  )