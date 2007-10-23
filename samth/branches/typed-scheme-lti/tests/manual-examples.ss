(module tlang mzscheme
  (require (prefix tl: (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))))
  (provide (all-from (planet "typed-scheme.ss" ("plt" "typed-scheme.plt")))))


(module even-odd (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define: (my-odd? [n : number]) : boolean
    (if (zero? n) #f
        (my-even? (- n 1))))

  (define: (my-even? [n : number]) : boolean
    (if (zero? n) #t
        (my-odd? (- n 1))))

  (display (my-even? 12)))

(module date (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  
  (define-typed-struct my-date ([day : number] [month : str] [year : number]))
  
  (define: (format-date [d : my-date]) : str
    (format "Today is day ~a of ~a in the year ~a" (my-date-day d) (my-date-month d) (my-date-year d)))
  
  (display (format-date (make-my-date 28 "November" 2006)))
  
  )

(module tree (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct leaf ([val : number]))
  (define-typed-struct node ([left : (Un node leaf)] [right : (Un node leaf)]))
  
  (define: (tree-height [t : (Un node leaf)]) : number
    (cond [(leaf? t) 1]
          [else (max (tree-height (node-left t))
                     (tree-height (node-right t)))]))
  
  (define: (tree-sum [t : (Un node leaf)]) : number
    (cond [(leaf? t) (leaf-val t)]
          [else (+ (tree-sum (node-left t))
                   (tree-sum (node-right t)))])))

(module tree (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct leaf ([val : number]))
  (define-typed-struct node ([left : (Un node leaf)] [right : (Un node leaf)]))
  
  (define-type-alias tree (Un node leaf))
  
  (define: (tree-height [t : tree]) : number
    (cond [(leaf? t) 1]
          [else (max (tree-height (node-left t))
                     (tree-height (node-right t)))]))
  
  (define: (tree-sum [t : tree]) : number
    (cond [(leaf? t) (leaf-val t)]
          [else (+ (tree-sum (node-left t))
                   (tree-sum (node-right t)))])))

(module add-list (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define: (sum-list [l : (list-of number)]) : number
    (cond [(null? l) 0]
          [else (+ (car l) (sum-list (cdr l)))])))

(module maybe (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define-typed-struct Nothing ())
  (define-typed-struct (a) Just ([v : a]))
  
  (define-type-alias (Maybe a) (Un Nothing (Just a)))
  
  (define: (find [v : number] [l : (list-of number)]) : (Maybe number)
    (cond [(null? l) (make-Nothing)]
          [(= v (car l)) (make-Just v)]
          [else (find v (cdr l))])))
  