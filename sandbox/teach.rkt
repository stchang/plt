#lang racket

(require lang/private/teach)
(require "stepper-properties.rkt")

;(syntax->datum (do-expansion '(beginner-or #f #f #t)))
#;(begin0
    (if (#%app verify-boolean '#f 'or)
        '#t
        (if (#%app verify-boolean '#f 'or)
            '#t
            (if (#%app verify-boolean '#t 'or) '#t '#f))))

#;(second ; gets to #f in first if test
 (syntax-e
  (cdr ; gets to (verify-boolean '#f 'or)
   (syntax-e
    (second ; gets to first if test
     (syntax-e 
      (second  ; gets to begin0 body (first if)
       (syntax-e 
        (do-expansion '(beginner-or #f #f #t))))))))))


;(syntax->datum (do-expansion '(beginner-define x 4)))
#;(begin
   (#%app check-top-level-not-defined 'define (quote-syntax x))
   (define-values (x) '4))
;(syntax->datum (do-expansion '(intermediate-define x 4)))
#;(begin
   (#%app check-top-level-not-defined 'define (quote-syntax x))
   (define-values (x) '4))

;beginner-define: function definitions are not allowed in the interactions window; they must be in the definitions window in: (beginner-define (f x) (+ x 1))
;(syntax->datum (do-expansion '(beginner-define (f x) (+ x 1))))

;(syntax->datum (do-expansion '(intermediate-define (f x) (+ x 1))))
#;(begin
   (#%app check-top-level-not-defined 'define (quote-syntax f))
   (define-values (f) (lambda (x) (#%app + x '1))))

;(syntax->datum (do-expansion '(beginner-define-struct pos (x y))))
#;(begin
   (#%app check-top-level-not-defined 'define-struct (quote-syntax pos))
   (#%app
    check-top-level-not-defined
    'define-struct
    (quote-syntax struct:pos))
   (#%app
    check-top-level-not-defined
    'define-struct
    (quote-syntax make-pos))
   (#%app check-top-level-not-defined 'define-struct (quote-syntax pos?))
   (#%app check-top-level-not-defined 'define-struct (quote-syntax pos-x))
   (#%app check-top-level-not-defined 'define-struct (quote-syntax pos-y))
   (begin
     (define-syntaxes
      (pos)
      (let-values (((g14216) (#%app syntax-local-certifier '#t)))
        (#%app
         list
         (#%app g14216 (quote-syntax struct:pos))
         (#%app g14216 (quote-syntax make-pos))
         (#%app g14216 (quote-syntax pos?))
         (#%app
          list
          (#%app g14216 (quote-syntax pos-y))
          (#%app g14216 (quote-syntax pos-x)))
         (#%app list '#f)
         '#t)))
     (begin
       (define-syntaxes
        (make-pos)
        (#%app
         make-first-order-function
         'constructor
         '2
         (quote-syntax make-pos)
         (quote-syntax #%app)))
       (define-syntaxes
        (pos?)
        (#%app
         make-first-order-function
         'predicate
         '1
         (quote-syntax pos?)
         (quote-syntax #%app)))
       (define-syntaxes
        (pos-x)
        (#%app
         make-first-order-function
         'selector
         '1
         (quote-syntax pos-x)
         (quote-syntax #%app)))
       (define-syntaxes
        (pos-y)
        (#%app
         make-first-order-function
         'selector
         '1
         (quote-syntax pos-y)
         (quote-syntax #%app)))
       (define-values
        (make-pos pos? pos-x pos-y)
        (let-values ()
          (letrec-values (((struct:pos
                            make-pos
                            pos?
                            pos-x
                            pos-y
                            set-pos-x!
                            set-pos-y!)
                           (let-values (((struct: make- ? -ref -set!)
                                         (let-values ()
                                           (let-values ()
                                             (#%app
                                              make-struct-type
                                              'pos
                                              '#f
                                              '2
                                              '0
                                              '#f
                                              null
                                              '#f
                                              '#f
                                              '()
                                              '#f
                                              'make-pos)))))
                             (#%app
                              values
                              struct:
                              make-
                              ?
                              (#%app make-struct-field-accessor -ref '0 'x)
                              (#%app make-struct-field-accessor -ref '1 'y)
                              (#%app make-struct-field-mutator -set! '0 'x)
                              (#%app
                               make-struct-field-mutator
                               -set!
                               '1
                               'y)))))
            (#%app values make-pos pos? pos-x pos-y)))))))


;(syntax->datum (do-expansion '(beginner-lambda (x) (+ x 1))))
;beginner-lambda: found a `lambda' expression that is not a function definition in: (beginner-lambda (x) (+ x 1))

;(syntax->datum (do-expansion 4))
;''4

;(syntax->datum (do-expansion true))
;''#t

;(syntax->datum (do-expansion '(beginner-app + 1 2)))
#;'(#%app + '1 '2)

#;(syntax->datum (do-expansion 
                '(beginner-cond 
                   [false (+ 1 2)] 
                   [true 5] 
                   [beginner-else 10])))
#;(begin0
    (if (#%app verify-boolean (#%top . false) 'cond)
        (begin (#%app + '1 '2))
        (if (#%app verify-boolean (#%top . true) 'cond)
            (begin '5)
            (if '#t
                (begin '10)
                (begin (#%app error 
                              'cond 
                              '"all question results were false"))))))


;(syntax->datum (do-expansion '(beginner-if (= 2 3) 4 5)))
;(begin0 (if (#%app verify-boolean (#%app = '2 '3) 'if) '4 '5))

;(syntax->datum (do-expansion '(beginner-quote/expr a)))
;''a


;; ---------------------------------------------------------------
;; Intermediate Macros
;; ---------------------------------------------------------------

