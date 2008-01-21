#lang planet plt typed-scheme.plt 3
1

(require (lib "match.ss"))

(define-type-alias NL (Listof Number))

(define: x : NL (list 1 2 3))

#{(match x
    [((? number? a) (? number? #{b : NL}) ...) (+ a (length b))]
    [_ 2]) :: Number}