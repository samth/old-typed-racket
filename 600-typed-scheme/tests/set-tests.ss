;; should FAIL!

(module set-tests "../typed-scheme.ss"
  
  (let*: ((x : Any 1) 
          (f : (-> Void) (lambda () (set! x (quote foo)))))
         (if (number? x) (begin (f) (add1 x)) 12))
  
  )