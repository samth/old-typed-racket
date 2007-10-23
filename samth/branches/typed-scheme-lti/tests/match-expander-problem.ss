#reader(planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module seasoned-schemer (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  #;(require (lib "etc.ss"))
  (require "prims.ss")
  (require (lib "match.ss"))
  
  (define-typed-struct pt ([x : number] [y : number]))
  
  (require-for-syntax mzscheme)
  

  (define-match-expander blah #:match (lambda (stx) (syntax-case stx ()
                                                      [(_ . a) #'($ . a)])))
  
  (define: (pt-add/match/blah [v : univ]) : number
    (match v
      [(blah pt #{x number} #{y number}) (+ x y)]
      [_ 0]))
  
 
  )