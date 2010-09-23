#lang racket

;(require "stepper-properties.rkt")
(define (do-expansion-lazy e)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'lazy/lazy)
    (expand
     (datum->syntax #f e))))

(syntax->datum (do-expansion-lazy
                'f))