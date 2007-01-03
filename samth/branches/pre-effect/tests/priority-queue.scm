;;; priority-queue.scm  --  Jens Axel Søgaard
;;; PURPOSE

; This file implements priority queues on top of
; a heap library.
#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))

(module priority-queue (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (require (prefix heap: "leftist-heap.ss")
           (all-except (lib "67.ss" "srfi") number-compare current-compare =? <?)
           (only "leftist-heap.ss" comparator))
  (require/typed number-compare (number number -> number) (lib "67.ss" "srfi"))
  (require/typed current-compare (-> (top top -> number)) (lib "67.ss" "srfi"))
  (require/typed =? ((top top -> number) top top -> boolean) (lib "67.ss" "srfi"))
  (require/typed <? ((top top -> number) top top -> boolean) (lib "67.ss" "srfi"))

  ; a priority-queue is a heap of  (cons <priority> <element>)
  
  (define-type-alias (elem a) (cons number a))
  
  (define-typed-struct (a) priority-queue ([heap : (heap:Heap (elem a))]))
  
  (define-type-alias (pqh a) (heap:Heap (elem a)))
  
  ; conveniences
  (pdefine: (a) (heap [pq : (priority-queue a)]) : (pqh a) (priority-queue-heap pq))
  (pdefine: (a) (pri [p : (elem a)]) : number (car p))
  (pdefine: (a) (elm [p : (elem a)]) : a  (cdr p))
  (pdefine: (a) (make [h : (pqh a)]) : (priority-queue a)  (make-priority-queue h))
  
  ; sort after priority
  ; TODO: and then element?
  (pdefine: (a) (compare [p1 : (elem a)] [p2 : (elem a)]) : number
    (number-compare (pri p1) (pri p2)))
  
  ;;; OPERATIONS
  
  (define: (num-elems [h : (heap:Heap (cons number number))]) : (list-of (cons number number))
    (heap:elements h))

  (pdefine: (a) (elements [pq : (priority-queue a)]) : (list-of a)
          (map #{elm :: ((elem a) -> a)} (heap:elements (heap pq))))

  (pdefine: (a) (elements+priorities [pq : (priority-queue a)]) : (values (list-of a) (list-of number))
    (let: ([eps : (list-of (elem a)) (heap:elements (heap pq))])
      (values (map #{elm :: ((elem a) -> a)} eps)
              (map #{pri :: ((elem a) -> number)} eps))))
  
  (pdefine: (a) (empty? [pq : (priority-queue a)]) : boolean
    (heap:empty? (heap pq)))

  (define: empty : (All (a) (case-lambda (-> (priority-queue a)) (comparator -> (priority-queue a))))
    (pcase-lambda: (a)
      [()    (empty (current-compare))]
      [([cmp : comparator]) (make #{(#{heap:empty :: (case-lambda (-> (pqh a))
                                                                  (comparator -> (pqh a)))} cmp) :: (pqh a)})]))
  
  (pdefine: (e r) (fold [f : (e r -> r)] [b : r] [a : (priority-queue e)]) : r
    (heap:fold f b (heap a)))
  

  ;; "bug" found - handling of empty heaps
  (pdefine: (a) (find-min [pq : (priority-queue a)]) : a 
    (let ([h (heap pq)])
      (if (heap:heap-node? h)
          (elm (heap:find-min h))
          (error "priority queue empty"))))

  (pdefine: (a) (find-min-priority [pq : (priority-queue a)]) : number
    (let ([h (heap pq)])
      (if (heap:heap-node? h)
          (pri (heap:find-min h))
          (error "priority queue empty"))))
  
  (pdefine: (a) (insert [x : a] [p : number] [pq : (priority-queue a)]) : (priority-queue a)
    (make (heap:insert (#{cons :: (case-lambda (a (list-of a) -> (list-of a)) (number a -> (cons number a)))} p x) (heap pq))))

  ;; FIXME - had to insert extra binding to give the typechecker more help
  ;; could have done this with annotation on map, probably
  (pdefine: (a) (insert* [xs : (list-of a)] [ps : (list-of number)] [pq : (priority-queue a)]) : (priority-queue a)
            (let ([cons #{cons :: (case-lambda (a (list-of a) -> (list-of a)) (number a -> (cons number a)))}])
              (make (heap:insert* (map #{cons :: (number a -> (cons number a))} ps xs) (heap pq)))))
  
  (pdefine: (a) (delete-min [pq : (priority-queue a)]) : (priority-queue a)
    (let ([h (heap pq)])
      (if (heap:heap-node? h)
          (make (heap:delete-min h)))
          (error "priority queue empty")))

  
  (pdefine: (a) (size [pq : (priority-queue a)]) : number
    (heap:size (heap pq)))
  
  (pdefine: (a) (union [pq1 : (priority-queue a)] [pq2 : (priority-queue a)]) : (priority-queue a)
    (make (heap:union (heap pq1) (heap pq2))))
  
  
  #;(require "signatures/priority-queue-signature.scm")
  #;(provide-priority-queue)
  )
