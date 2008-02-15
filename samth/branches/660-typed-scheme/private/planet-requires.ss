#lang scheme/base

(require
 (for-syntax scheme/base
             scheme/require-transform
             mzlib/etc)
 (planet "require.ss" ("ryanc" "require.plt" 1 2)))

(define-module schemeunit 
  (planet "test.ss" ("schematics" "schemeunit.plt" 2 3))
  (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 3))
  (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 3))
  (planet "util.ss" ("schematics" "schemeunit.plt" 2 3))
  (planet "random.ss" ("cce" "fasttest.plt" 1 1))
  (planet "schemeunit.ss" ("cce" "fasttest.plt" 1 1)))


(define-syntax require-begin
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ . rest)
        (begin-with-definitions
          ;; transform : Listof[Cons[A,B]] -> Values[Listof[A],Listof[B]]
          (define (transform l)
            (let loop ([a null] [b null] [l l])
              (cond [(null? l) (values (reverse a) (reverse b))]
                    [else (loop (cons (car (car l)) a) (cons (cdr (car l)) b) (cdr l))])))
          ;; e is a Listof[Cons[Listof[Import],Listof[ImportSrc]]]
          (define e (map (lambda (e) (define-values (a b) (expand-import e)) (cons a b))
                         (syntax->list #'rest)))
          ;; es : Listof[Listof[Import]]
          ;; es* : Listof[Listof[ImportSrc]]
          (define-values (es es*) (transform e))                   
          (values (apply append es) (apply append es*)))]))))

(require (require-begin (prefix-in set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))))

(define-module galore
  (prefix set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))
  (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3 3))))

(define-module libs
  (prefix set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))
  (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3 3)))
  (planet "equiv.ss" ("cce" "equiv.plt" 1 0))
  (lib "match.ss")
  (all-except (lib "list.ss") remove)
  (lib "etc.ss")
  (lib "trace.ss")
  (lib "struct.ss"))

