#lang racket

(require string-constants
         drracket/tool
         lang/stepper-language-interface)
  
  (provide tool@)
  
  (define tool@
    (unit 
      (import drracket:tool^)
      (export drracket:tool-exports^)


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
           ((drracket:language:get-default-mixin)
            (drracket:language:module-based-language->language-mixin
             (drracket:language:simple-module-based-language->module-based-language-mixin
              drracket:language:simple-module-based-language%)))))
        
        (drracket:language-configuration:add-language
         (instantiate lazy-language% ()
           (one-line-summary '("Lazy Racket"))
           (module '(lib "lazy/lazy.rkt"))
           (language-position `(,(string-constant experimental-languages) "Lazy Racket"))
           (language-numbers '(1000 -500))
           (stepper:supported #t)
           (stepper:enable-let-lifting #t)
           (stepper:show-lambdas-as-lambdas #t)))
        )))
