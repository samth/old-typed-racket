(module info (lib "infotab.ss" "setup")
  (define name "Typed Scheme")

  (define html-docs (list "doc"))
  (define doc.txt "doc.txt")
 
  (define tools (list #;"private/tool.ss"))
  (define tool-names (list #;"Typed Scheme"))
  (define compile-omit-files (list "private/660-env.ss" "coverage.ss" "typed-scheme-660.scm" "660-lang.ss"))
  
  (define categories (list 'devtools 'metaprogramming))
  
  (define can-be-loaded-with 'all)
  
  (define homepage "http://www.ccs.neu.edu/home/samth/typed-scheme.html")
  
  (define primary-file "typed-scheme.ss")
  (define required-core-version "371")
  (define version "0.91")
  
  (define release-notes '("This release fixes an error that occurred when errortrace was enabled."
                          "This release fixes several regressions in the previous release."))
  
  (define blurb `("This language allows the definition and execution of Typed Scheme programs."))
  
  )

