;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname beginner-expand-test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;"\Program Files\Racket-Full-5.0.0.1"\mzc --expand beginner-expand-test.rkt
;(or #f #f #t)

(define-struct pos (x y))
(define (f x) (+ x 1))
(+ (f (pos-x (make-pos 1 2)))
   (f (pos-y (make-pos 3 4))))

(cond
  [false 1]
  [false 2]
  [else 3])