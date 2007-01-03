(module info (lib "infotab.ss" "setup")
  (define name "Typed Scheme")

  (define html-docs (list "doc"))
  (define doc.txt "doc.txt")
 
  (define tools (list "private/tool.ss"))
  (define tool-names (list "Typed Scheme"))
  (define compile-omit-files (list))
  
  (define categories (list 'devtools 'metaprogramming))
  
  (define can-be-loaded-with 'all)
  
  (define homepage "http://www.ccs.neu.edu/home/samth/typed-scheme")
  
  (define primary-file "typed-scheme.ss")
  (define required-core-version "369.1")
  (define version "0.1")
  
  (define blurb `("This language allows the definition and execution of typed scheme programs."))
  
  )

