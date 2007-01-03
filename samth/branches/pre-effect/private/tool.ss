(module tool mzscheme
  (require (lib "unit.ss")
	   (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "list.ss")
	   (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants"))
  
  (require (prefix ts: "../typed-scheme.ss")
           "typechecker.ss")

  (provide tool@)
  
  (define tool@
    (unit
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define mbl% (drscheme:language:module-based-language->language-mixin
                    (drscheme:language:simple-module-based-language->module-based-language-mixin 
                     drscheme:language:simple-module-based-language%)))
      
      (define planet-module '(planet "typed-scheme.ss" ("plt" "typed-scheme.plt")))
      
      (define (get-filename-from-path s)
        (define-values (_ name __) (split-path (string->path s)))
        name)
      
      ;; COPIED FROM collects/drscheme/private/module-language.ss revision 5083
      ;; FIXME!!!!      
      ;; get-filename : port -> (union string #f)
      ;; extracts the file the definitions window is being saved in, if any.
      (define (get-filename port)
        (let ([source (object-name port)])
          (cond
            [(path? source) (path->string source)]
            [(is-a? source text%) (send source get-filename/untitled-name)
             #;(let ([canvas (send source get-canvas)])
               (and canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (and (is-a? frame drscheme:unit:frame%)
                           (let* ([b (box #f)]
                                  [filename (send (send frame get-definitions-text)
                                                  get-filename
                                                  b)])
                             (if (unbox b)
                                 #f
                                 filename))))))]
            [else #f])))        

      (define typed-scheme-language%
	(class* mbl% (drscheme:language:simple-module-based-language<%>)
	  (define/override (get-language-numbers)
	    '(1000 -401))
	  (define/override (get-language-position)
	    (list (string-constant experimental-languages) "Typed Scheme"))
	  (define/override (get-module) planet-module)
 	  (define/override (get-one-line-summary)
	    "Scheme with types!")
          (define/override (get-language-url) #f)
	  (define/override (get-reader) 
            (lambda (name port)
	      (let ([v (ts:read-syntax name port)])
		(if (eof-object? v)
		    v
		    (namespace-syntax-introduce v)))))  
          (define/override (front-end/complete-program port setting tc)
            (define forms
              (let loop ((acc '()))
                (let ([v ((get-reader) (object-name port) port)])
                  (if (eof-object? v) (reverse acc) (loop (cons v acc))))))
            (define ctxt (if (pair? forms) (car forms) #'here))
            (define module-name (datum->syntax-object 
				 ctxt 
				 (string->symbol (path->string (get-filename-from-path (get-filename port))))))
            (define mod
              #`(module #,module-name #,planet-module
                  #,@forms))
            (let ([to-go (list mod #`(eval '(require #,module-name)) #`(eval (current-namespace (module->namespace '#,module-name))))])
              (lambda () (if (null? to-go) eof (begin0 (car to-go) (set! to-go (cdr to-go))))))
            )
	  (super-new [module (get-module)] [language-position (get-language-position)])))
      
      (define (phase1) (void))
      (define (phase2)
        (drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) 
                         typed-scheme-language%))))))
  )
      
      
  
  
  