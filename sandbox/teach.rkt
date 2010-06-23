#lang racket

(require lang/private/teach)
(require "stepper-properties.rkt")

#;(begin0
    (if (#%app verify-boolean '#f 'or)
        '#t
        (if (#%app verify-boolean '#f 'or)
            '#t
            (if (#%app verify-boolean '#t 'or) '#t '#f))))

(second ; gets to #f in first if test
 (syntax-e
  (cdr ; gets to (verify-boolean '#f 'or)
   (syntax-e
    (second ; gets to first if test
     (syntax-e 
      (second  ; gets to begin0 body (first if)
       (syntax-e 
        (do-expansion '(beginner-or #f #f #t))))))))))