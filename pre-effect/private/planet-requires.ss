(module planet-requires mzscheme
  (require (planet "require.ss" ("ryanc" "require.plt" 1)))  
  
  (define-module schemeunit 
    (planet "test.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 3))
    (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 3)))

  (define-module libs
    (prefix set: (planet "set.ss" ("soegaard" "galore.plt" 3 3)))
    (planet "equiv.ss" ("cce" "equiv.plt" 1 0))
    (lib "match.ss")
    (all-except (lib "list.ss") remove)
    (lib "etc.ss")
    (lib "trace.ss")
    (lib "struct.ss")
    )
  
  )
