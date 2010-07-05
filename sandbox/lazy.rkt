#lang racket

;(require lazy)
(require "stepper-properties.rkt")

;(define (f x) (+ 1 x))
;(cdr (cdr ;; lambda body (#%app lazy (lambda () ...))
;(syntax-e
;(car ; (lambda (x) ...)
; (cdr
; (syntax-e 
;  (cdr ; (lazy-proc (lambda ...))
;   (syntax-e 
    (third ; (#%app lazy-proc (lambda ...))
     (syntax-e 
      (do-expansion-lazy '(define (f x) (+ x 1)))))


#;(syntax->datum (do-expansion-lazy
;                '(begin 
;                   (let ([f (λ (x) (+ x 1))])
                     '(f 4)))

(require stepper/private/shared)
(require stepper/private/my-macros)
(require stepper/private/annotate)


(define free-vars-captured #f)
;(skipto/auto
;(second (syntax-e (cdr (syntax-e 
#;(update
  (append skipto/cdr skipto/second)
  (third ; (#%app lazy-proc (lambda ...))
  (syntax-e 
   (do-expansion-lazy '(define (f x) (+ x 1)))))
  (lambda (x) x)
  'discard)
; 'rebuild
; (lambda (subterm) subterm))

