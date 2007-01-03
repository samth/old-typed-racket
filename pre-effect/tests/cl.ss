#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module cl (planet "typed-scheme.ss" ("plt" "typed-scheme.plt"))
  (define: f : (case-lambda [number -> number] [boolean boolean -> boolean])
    (case-lambda [(#{a : number}) a]
                 [(#{b : boolean} #{c : boolean}) (and b c)])))
