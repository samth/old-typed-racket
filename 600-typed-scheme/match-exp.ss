#lang planet plt typed-scheme.plt 3 0
1


(require (lib "plt-match.ss"))



(let-values (((x) (list 3))) 
  (let-values (((match-failure) (lambda () (#%app error))))
    (if (#%app pair? x)
        (let-values (((exp19087) (#%app cdr x)))
          (if (#%app list? exp19087)
              (#%app (letrec-values (((loop19088)
                                      (lambda (exp19089 b1)
                                        (if (#%app null? exp19089)
                                            (let-values 
                                                (((#{b : (Listof Number)}) (#%app reverse b1))
                                                 ((a) (#%app car x)))
                                              (#%datum . 1))
                                            (#%app loop19088
                                                   (#%app cdr exp19089) ;; this can't type, because we know too much about the variable
                                                   (#%app cons (#%app car exp19089) b1))))))
                       loop19088)
                     exp19087 (quote ()))
              (let-values () (#%datum . 2))))
        (let-values () 2))))

#;

#{(match (list 3)
  [(list a #{b : (Listof Number)} ...) 1]
  [_ 2]) :: Number}

