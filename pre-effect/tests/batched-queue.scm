#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module batched-queue (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  ;; CHANGES
  ;; added annotations on all bound variables and structs
  ;; require typed foldl
  ;; made empty into a nullary function
  ;; added annotations on empty lists
  ;; added annotation on use of polymorphic functions in higher-order contexts
  
  ;; fixme -- how do we require polymorphic functions?
  #;(require (only (lib "list.ss") foldl)) 
  (require (only "typed-list.ss" foldl))
  (define-typed-struct (a) queue ([front : (list-of a)] [rear : (list-of a)]))
  
  ; Invariants
  ;   1.  q empty <=> (null? (queue-front q))
  ;   2.  elements of q  =  (append (queue-front q) (reverse (queue-rear q)))
  
  
  ;; fixme -- shouldn't have to be a function
  (pdefine: (a) (empty) : (queue a) (make-queue #{'() : (list-of a)} #{'() : (list-of a)}))
  
  (pdefine: (a) (empty? [q : (queue a)]) : boolean
    (null? (queue-front q)))
  
  (pdefine: (a) (insert-last [x : a] [q : (queue a)]) : (queue a)
    (let ([front (queue-front q)])
      (if (null? front)
          (make-queue #{(cons x front) :: (Listof a)} #{'() :: (Listof a)})
          (make-queue #{front :: (Listof a)} #{(cons x (queue-rear q)) :: (Listof a)}))))
  
  (define:  insert : (All (a) (a (queue a) -> (queue a))) insert-last)
  
  (pdefine: (a) (insert* [xs : (list-of a)] [q : (queue a)]) : (queue a)
            ;; fixme - annoying annotation
            (foldl #{insert :: (a (queue a) -> (queue a))} q xs))
  
  (pdefine: (a) (remove-first [q : (queue a)]) : (queue a)
    (let ([front (queue-front q)])
      (if (null? front)
          (error 'remove-first "can't remove element from empty queue; given " q)
          (if (null? (cdr front))
              (make-queue (reverse (queue-rear q)) #{'() : (list-of a)})
              (make-queue #{(cdr front) :: (Listof a)} (queue-rear q))))))
  
  (pdefine: (a) (first+remove [q : (queue a)]) : (values a (queue a))
    (let ([front (queue-front q)])
      (if (null? front)
          (error 'remove-first "can't remove element from empty queue; given " q)
          (values (car front)
                  (if (null? (cdr front))
                      (make-queue (reverse (queue-rear q)) #{'() : (list-of a)})
                      (make-queue #{(cdr front) :: (Listof a)} (queue-rear q)))))))
  
  (define: remove : (All (a) ((queue a) -> (queue a))) remove-first)
  
  (pdefine: (a) (first [q : (queue a)]) : a
    (if (empty? q)
        (error 'first "There is no first element in an empty queue; given " q))
    (car (queue-front q)))
  
  (pdefine: (a) (elements: [q : (queue a)]) : (list-of a)
    (append (queue-front q) 
            (reverse (queue-rear q))))
    
  (pdefine: (a b) (fold [f : (a b -> b)] [init : b] [q : (queue a)]) : b
    (foldl f 
           (foldl f init (queue-front q))
           (reverse (queue-rear q))))
  
  (pdefine: (a) (size [q : (queue a)]) : number
    ; NOTE: T(size) = O(n)
    (+ (length (queue-front q))
       (length (queue-rear q))))
  
  ;; 12 definitions checked 
  ;; generators removed
  
  ;; TESTS
  
  (= 0 (size (empty)))
  

)