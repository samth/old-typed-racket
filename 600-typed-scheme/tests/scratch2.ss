#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module scratch2 "../typed-scheme.ss"
  
  (define-type Tree 
    [Leaf (v number)]
    [Node (l Tree) (r Tree)])
  
  (define: (sum [x : Tree]) : number 
    (if (Node? x)
        (+ (sum (Node-l x)) (sum (Node-r x)))
        (Leaf-v x)))
  
  (define: x : Tree (Leaf 3))
  
  (define: x* : Tree (Node x x))
  
  (printf "sum x*: ~a~n" (sum x*))
  
  (cases (Node x x)
    [(Node (Leaf n) (Leaf m)) (display (+ m n))]
    [else (display 'other)])
  (newline)
  
  (display (let: ([x : top x*])
    (cases x
      [(Node a b) 1]
      [(Leaf n) 2])))
  
  
  
  (define: y : Node 
    (if (Node? x*) x* (error 'foo)))
  
    
  (define-type RBTree 
    [RBLeaf (v number)]
    [RNode (l RBTree) (v number) (r RBTree)]
    [BNode (l RBTree) (v number) (r RBTree)])
  
  (define-type-alias RTree (U RBLeaf RNode))
  
  )