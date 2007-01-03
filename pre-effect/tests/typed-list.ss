#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module typed-list (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (provide foldl)
  
  (pdefine: (a b) (foldl [f : (a b -> b)] [e : b] [l : (list-of a)]) : b
            (if (null? l) e
                (foldl f (f (car l) e) #{(cdr l) :: (Listof a)})))

  )