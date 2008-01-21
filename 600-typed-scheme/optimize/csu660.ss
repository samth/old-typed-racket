(module csu660 mzscheme
  (require (lib "list.ss") (lib "etc.ss") (lib "trace.ss")
           "datatype.ss" "utils.ss")
  (provide
   
   
   ;; =========================================================================
   ;; mzscheme - syntax requirements
   #%app #%datum #%top #;#%module-begin #;#%plain-module-begin #;#%top-interaction
   ;; make it possible to use additional modules
   ;; require
   ;; --- basic scheme
   + - * / < <= = > >= abs acos add1 and andmap angle append arithmetic-shift
   asin assoc assq assv atan bitwise-and bitwise-ior bitwise-not bitwise-xor
   boolean? caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar
   caddar cadddr caddr cadr car case cdaaar cdaadr cdaar cdadar cdaddr cdadr
   cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling complex? cond
   cos define denominator eq? equal? eqv? even? exact->inexact exact? exp expt
   floor format gcd imag-part inexact->exact inexact? integer? lcm length let
   let* list list* list-ref list-tail log magnitude map max member memq memv
   min modulo negative? not null null? number->string number? numerator odd? or
   ormap pair? positive? procedure? quasiquote quote quotient random rational?
   rationalize real-part real? remainder reverse round sin sqrt string
   string->number string-append string-ci<=? string-ci<? string-ci=?
   string-ci>=? string-ci>? string-length string-ref string<=? string<?
   string=? string>=? string>? string? sub1 substring symbol? tan truncate
   unquote unquote-splicing zero?
   ;; --- expose normal error
   error
   ;; --- advanced HOF
   apply lambda letrec
   ;; --- simple side effects
   display newline print printf write read-line
   sleep time
   begin begin0 for-each unless when
   ;; --- boxes
   box box? set-box! unbox
   ;; --- vectors
   make-vector list->vector vector? vector-ref
   ;; --- syntax
   define-syntax syntax-rules
   ;; --- used at the very end
   set!
   ;; =========================================================================
   ;; list
   first second third fourth fifth sixth seventh eighth rest cons? empty empty?
   foldl foldr last-pair remove remq remv remove* remq* remv* memf assf filter
   sort
   ;; =========================================================================
   ;; etc
   true false boolean=? symbol=? identity compose build-list build-vector
   ;; =========================================================================
   ;; trace
   trace untrace
   ;; =========================================================================
   ;; datatype
   ;define-type cases
   ;; =========================================================================
   ;; utils

   (rename *if if)
   (rename *cons cons) (rename *list? list?)
   (rename cons cons*) ; make this available for lazy evaluators
   match number: integer: symbol: string: boolean:
   string->sexpr make-transformer test test-mode
   any? list-of sexp-of box-of union-of intersection-of false? true?
   ))
