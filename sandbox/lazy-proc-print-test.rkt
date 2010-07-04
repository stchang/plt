#lang racket

(define-values (lazy-proc lazy-proc?)
  (let-values ([(type make pred ref set)
                (make-struct-type
                 'lazy-proc #f 1 0 #f null (current-inspector) 0)])
    (values make pred)))

  
(define f #'(lambda (x) x))
  
;(eval-syntax #`(lazy-proc #,f))
;(eval-syntax f)