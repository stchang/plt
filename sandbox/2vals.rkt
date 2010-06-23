#lang racket

(require stepper/private/my-macros)

(define x (2vals 1 2))

; => 6
(let*-2vals
 ([(y z) x])
 (* 2 (+ y z)))

; => '#((2 3 4) (0 1 2))
(2vals-map (λ (x) (2vals (add1 x) (sub1 x))) '(1 2 3))

; => '#((2 3 4) (0 1 2))
(let ([lst (map (λ (x) (2vals (add1 x) (sub1 x))) '(1 2 3))])
  (vector (map (λ (x) (vector-ref x 0)) lst)
          (map (λ (x) (vector-ref x 1)) lst)))


; error
;(map (λ (x) (values (add1 x) (sub1 x))) '(1 2 3))