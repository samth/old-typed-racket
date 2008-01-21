#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module scratch "../typed-scheme.ss"

  (let: ((x : Any 3)) (if (number? x) (add1 x) 12))
  (let: ((v : (Un number boolean) #f)) (if (boolean? v) 5 (+ v 1)))
  
  )
