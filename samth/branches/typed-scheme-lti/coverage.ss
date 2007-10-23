(coverage-counts-enabled #t)
(require "tests/all-tests.ss")
(fast)
(printf "ran the tests~n")
(let loop ([n 0])
  (if (= n (vector-length argv))
      #f
      (begin
	(current-output-port (open-output-file
			      (string-append (vector-ref argv n) ".cover")
			      'replace))
	(annotate-covered-file (vector-ref argv n))
	(loop (add1 n)))))
	
