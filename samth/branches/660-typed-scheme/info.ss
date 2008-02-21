(module info (lib "infotab.ss" "setup")
  (define name "Typed Scheme")

  (define tools (list #;"private/tool.ss"))
  (define tool-names (list #;"Typed Scheme"))
  (define categories (list 'devtools 'metaprogramming))
  
  (define can-be-loaded-with 'all)
  
  (define homepage "http://www.ccs.neu.edu/home/samth/typed-scheme")
  
  (define primary-file "lang/main.ss")
  (define required-core-version "399")
  (define version "0.91")
  
  (define release-notes '("This release fixes an error that occurred when errortrace was enabled."
                          "This release fixes several regressions in the previous release."))
  
  (define blurb `("This language allows the definition and execution of Typed Scheme programs."))
  
  )

