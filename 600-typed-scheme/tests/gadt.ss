#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module gadt (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  #|
data Exp a =
Num :: Int -> Exp Int
Sum :: Int -> Int -> Exp Int
Zero :: Exp Int -> Exp Bool
   |#
  
  #;(define-typed-struct (a) Exp ([flag : a]))
  (define-typed-struct (a) Num ([v : number]))
  (define-typed-struct (a) Zero ([e : (Un (Num number) (Zero number))]))
  
  (define-type-alias (Expr a) (Un (Num a) (Zero a)))
  
  (pdefine: (a) (ev [x : (Expr a)]) : a
    (cond
      [(Num? x) (Num-v x)]
      [(Zero? x) (= 0 #{(#{ev :: (All (b) ((Expr b) -> b))} #{(Zero-e x) :: (Expr number)}) :: number})]))
  
  )