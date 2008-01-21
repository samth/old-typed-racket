(module planet-requires mzscheme
  (require (planet "require.ss" ("ryanc" "require.plt" 1 2)))
  
  (define-module schemeunit 
    (planet "test.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "util.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "random.ss" ("cce" "fasttest.plt" 1 1))
    (planet "schemeunit.ss" ("cce" "fasttest.plt" 1 1)))

  
  (define-module galore
    (prefix set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))
    (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3 3))))
  
  
  (define-module libs
    (prefix set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))
    (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3 3)))
    (planet "equiv.ss" ("cce" "equiv.plt" 1 0))
    (lib "match.ss")
    (all-except (lib "list.ss") remove)
    (lib "etc.ss")
    (lib "trace.ss")
    (lib "struct.ss"))
    
  
  )
