#lang scheme/base

(require (lib "boundmap.ss" "syntax")
         "tc-utils.ss")

(provide register-type
         lookup-type
         register-types
         type-env-map)

;; module-identifier-mapping from id -> type
;; where id is a variable, and type is the type of the variable
(define the-mapping (make-module-identifier-mapping))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type id type)
  #;(printf "registering ~a~n" (syntax-object->datum id))
  (module-identifier-mapping-put! the-mapping id type))

;; add a bunch of types to the mapping
;; listof[id] listof[type] -> void
(define (register-types ids types)
  (for-each register-type ids types))

;; given an identifier, return the type associated with it
;; if none found, calls lookup-fail
;; identifier -> type 
(define (lookup-type id [fail-handler (lambda () (lookup-fail (syntax-e id)))])
  #;(printf "looking up ~a~n" (syntax-e id))
  (module-identifier-mapping-get the-mapping id fail-handler))  

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]  
(define (type-env-map f)
  (module-identifier-mapping-map the-mapping f))
