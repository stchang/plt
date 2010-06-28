#lang racket

;(require lazy)
(require "stepper-properties.rkt")

;(define (f x) (+ 1 x))
(second ; (lambda ...)
 (syntax-e 
  (cdr ; (lazy-proc (lambda ...))
   (syntax-e 
    (third ; (#%app lazy-proc (lambda ...))
     (syntax-e 
      (do-expansion-lazy '(define (f x) (+ x 1)))))))))


(syntax->datum (do-expansion-lazy
;                '(begin 
;                   (let ([f (λ (x) (+ x 1))])
                     '(f 4)))