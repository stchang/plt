#lang s-exp syntax/module-reader
lazy
#:info make-info

(define (make-info key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (list (dynamic-require 'stepper/drracket-button 
                            'stepper-drracket-button)
           (dynamic-require 'drracket/syncheck-drracket-button 
                            'syncheck-drracket-button))]
    [(drscheme:opt-out-toolbar-buttons) #f] ; opt out of extra buttons
    [else (use-default key default)]))