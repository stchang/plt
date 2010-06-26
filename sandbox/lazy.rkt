#lang racket

;(require lazy)
(require "stepper-properties.rkt")

(define (f x) (+ 1 x))
;(define-values (f) (lambda (x) (#%app + '1 x)))

(syntax->datum (do-expansion-lazy
;                '(begin 
;                   (let ([f (λ (x) (+ x 1))])
                     '(f 4)))