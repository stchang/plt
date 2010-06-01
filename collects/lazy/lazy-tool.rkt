#lang scheme
(require string-constants
;         framework
;         (prefix-in et: errortrace/stacktrace)
;         mzlib/pretty
;         (prefix-in pc: mzlib/pconvert)
;         mzlib/file
;         mzlib/unit
;         mzlib/class
;         mzlib/list
;         mzlib/struct
;         mzlib/compile
;         mzlib/struct
         drscheme/tool
;         mred
;         framework/private/bday
;         syntax/moddep
;         mrlib/cache-image-snip
;         compiler/embed
;         wxme/wxme
;         setup/dirs
         
         ;; this module is shared between the drracket's namespace (so loaded here) 
         ;; and the user's namespace in the teaching languages
;         "private/set-result.ss"
         
         lang/stepper-language-interface
;         "debugger-language-interface.ss"
;         "run-teaching-program.ss"
;         stepper/private/shared
         
;         (only-in test-engine/scheme-gui make-formatter)
;         (only-in test-engine/scheme-tests scheme-test-data error-handler test-format test-execute)
;         (lib "test-engine/test-display.scm")
         )
  
  
  (provide tool@)
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)


      (define (stepper-settings-language %)
        (if (implementation? % stepper-language<%>)
            (class* % (stepper-language<%>)
              (init-field stepper:supported)
              (init-field stepper:enable-let-lifting)
              (init-field stepper:show-lambdas-as-lambdas)
              (define/override (stepper:supported?) stepper:supported)
              (define/override (stepper:enable-let-lifting?) stepper:enable-let-lifting)
              (define/override (stepper:show-lambdas-as-lambdas?) stepper:show-lambdas-as-lambdas)
              (super-new))
            (class* % ()
              (init stepper:supported)
              (init stepper:enable-let-lifting)
              (init stepper:show-lambdas-as-lambdas)
              (super-new))))
      
      
      (define (phase1) (void))
      
      ;; phase2 : -> void
      (define (phase2)
        
        (define lazy-language%
          (stepper-settings-language
           ((drscheme:language:get-default-mixin)
            (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              drscheme:language:simple-module-based-language%)))))
        
        (drscheme:language-configuration:add-language
         (instantiate lazy-language% ()
           (one-line-summary '("Lazy Racket"))
           (module '(lib "lazy/lazy.rkt"))
           (language-position `(,(string-constant experimental-languages) "Lazy Racket"))
           (language-numbers '(1000 -500))
           (stepper:supported #t)
           (stepper:enable-let-lifting #t)
           (stepper:show-lambdas-as-lambdas #t)))
        )))
