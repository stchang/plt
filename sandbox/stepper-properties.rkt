#lang racket

(require stepper/private/shared)
(require test-engine/scheme-tests)

(provide do-expansion do-expansion-syntax)

#;(check-expect
 (syntax->datum
  (skipto/auto (stepper-syntax-property 
                #`(a #,(stepper-syntax-property #`(b c)
                                                'stepper-skipto
                                                '(syntax-e cdr car)))
                'stepper-skipto
                '(syntax-e cdr car))
               'discard
               (lambda (x) x)))
 'c)

(define (do-expansion e)
  ;(syntax->datum
   (parameterize ([current-namespace (make-base-namespace)])
     (namespace-require 'lang/private/teach)
     (expand
      (datum->syntax #f e))))

(define (do-expansion-syntax e)
  (syntax->datum
   (parameterize ([current-namespace (make-base-namespace)])
     (expand-syntax
      (datum->syntax #f e)))))

;; #lang racket expansions
#;(check-expect
 (do-expansion '(+ 1 2))
 '(#%app + '1 '2))

#;(check-expect
 (do-expansion '(define x 3))
 '(define-values (x) (quote 3)))

#;(check-expect
 (do-expansion '(define (f x) (+ 1 x)))
 '(define-values (f) (lambda (x) (#%app + (quote 1) x))))

#;(check-expect
 (do-expansion '(λ (x) (+ x 1)))
 '(#%expression (lambda (x) (#%app + x (quote 1)))))

#;(check-expect
 (do-expansion '(λ (x y) (λ (z) (+ x y z))))
 '(#%expression (lambda (x y) (lambda (z) (#%app + x y z)))))

#;(check-expect
 (do-expansion 'x)
 '(#%top . x))

#;(check-expect
 (do-expansion '(begin x y))
 '(begin (#%top . x) (#%top . y)))

#;(check-expect
 (do-expansion '(module test racket (define x 10)))
 '(module test racket (#%module-begin (define-values (x) (quote 10)))))

#;(check-expect
 (do-expansion '(module test racket 1))
 '(module test racket 
    (#%module-begin 
     (#%app call-with-values (lambda () (quote 1)) print-values))))

#;(check-expect
 (do-expansion '(module test racket 1 2 3))
 '(module test racket 
    (#%module-begin 
     (#%app call-with-values (lambda () (quote 1)) print-values) 
     (#%app call-with-values (lambda () (quote 2)) print-values) 
     (#%app call-with-values (lambda () (quote 3)) print-values))))

#;(check-expect
 (do-expansion '(module test racket (define (f x) (+ 1 x))))
 '(module test racket 
    (#%module-begin 
     (define-values (f) (lambda (x) (#%app + (quote 1) x))))))

#;(check-expect
 (do-expansion '(module test racket (define (f x) (+ x 1)) (f 4)))
 '(module test racket
    (#%module-begin 
     (define-values (f) (lambda (x) (#%app + x (quote 1)))) 
     (#%app call-with-values (lambda () (#%app f (quote 4))) print-values))))

#;(check-expect
 (do-expansion '(let ([x 1] [y 2]) (+ x y)))
 '(let-values (((x) (quote 1)) ((y) (quote 2))) (#%app + x y)))

#;(check-expect
 (do-expansion '(let* ([x 1] [y (+ x 1)]) (+ x y)))
 '(let-values (((x) (quote 1))) 
    (let-values (((y) (#%app + x (quote 1)))) 
      (#%app + x y))))


;; not working bc temp ids are different each time
#;(check-expect
 (do-expansion '(with-syntax ([x 1]) (syntax x)))
 '(let-values 
      (((ws9534) (#%app datum->syntax (quote-syntax here) (quote 1)))) 
    (let-values 
        (((arg) ws9534)) 
      (let-values 
          (((rslt) arg)) 
        (if (quote #t) 
            (let-values 
                (((sc9535) rslt)) 
              (let-values () sc9535)) 
            (let-values 
                (((rslt) (#%app (lambda (e) null) arg))) 
              (if rslt 
                  (let-values () 
                    (let-values () 
                      (#%app with-syntax-fail (quote-syntax x)))) 
                  (#%app raise-syntax-error (quote #f) (quote bad syntax) arg))))))))


(test)