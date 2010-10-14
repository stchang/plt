;(module test lazy
; eli's example
;(define (h x) (+ x (second (list (/ 9 0) (+ x x) (car '())))))
;(h (+ 1 2))

; (* 3 (second (map (lambda (n) (/ 1 n)) (list 0 1 2))))

;(define (f x) (+ x x))
;(f (f (+ 1 2)))

;(cadr (cons (/ 1 0) (cons (+ 1 2) '())))

(define (f x) (+ (car x) (car x)))
(f (list (+ 1 2) (+ 3 4)))
(define (g x) (+ (cadr x) (cadr x)))
(g (take 2 (list 1 2 3)))


;(define (f x) (append (!! x) x))
;(f (cdr (take 2 (list 1 2 3 4 5))))

;(cadr (cddr (list 1 2 3 4 5)))
;(cadr (cddr (cddr (cddr (list 1 2 3 4 5 6 7 8)))))
;(+ (cadr (cddr (list 1 2 3 4))) (cadr (cddr (list 5 6 7 8))))
;(cadr (cddr (list (+ 2 3 ) (+ 4 5) (+ 5 6) (+ 7 8))))

;(cadr (list (/ 1 0) (+ 1 2)))

;(define (f x y) (cadr (list x y x))) (f (/ 1 0) (+ 1 2))

;(! (if #f 2 3))
;(cadr (list 1 (if (+ 4 5) 2 3)))

;(for-each (lambda (x) x) '(1 2 3))

;(define ones (cons 1 ones))
;(define (f x) x)
;(f ones)

;(define (f x) (+ x (+ x x)))
;(define (g y) (+ (+ y y) y))
;(+ (f (+ 1 2)) (g (+ 3 4)))
;(f (+ (+ 1 2) (+ 3 4)))
;)
;(cadr (cons (/ 1 0) (cons (+ 1 2) null)))
 
 
#;(module test lazy
   (#%plain-module-begin
   (#%require (for-syntax scheme/mzscheme))
   
   (define-values
    (f)
    (#%app
     lazy-proc
     (lambda (x)
       (#%app
        lazy
        (lambda ()
          (#%app
           (lambda (p temp1 x2)
             (if (if (#%app lazy-proc? p)
                   '#t
                   (#%app struct-constructor-procedure? p))
               (#%app p temp1 x2)
               (#%app p (#%app ! temp1) (#%app ! x2))))
           (#%app ! +)
           '1
           x))))))
   
   (#%app
    (#%app toplevel-forcer)
    (#%app
     (lambda (p temp3)
       (if (if (#%app lazy-proc? p)
             '#t
             (#%app struct-constructor-procedure? p))
         (#%app p temp3)
         (#%app p (#%app ! temp3))))
     (#%app ! f)
     '10)) ))

; (define (f x) (+ x x))
; (f (+ 1 10))

  #;(module test lazy
  (#%plain-module-begin
   (#%require (for-syntax scheme/mzscheme))
   
   (define-values
    (f)
    (#%app
     lazy-proc
     (lambda (x)
       (#%app
        lazy
        (lambda ()
          (#%app
           (lambda (p x1 x2)
             (if (if (#%app lazy-proc? p)
                   '#t
                   (#%app struct-constructor-procedure? p))
               (#%app p x1 x2)
               (#%app p (#%app ! x1) (#%app ! x2))))
           (#%app ! +)
           x
           x))))))
   
   (#%app
    (#%app toplevel-forcer)
    (#%app
     (lambda (p temp3)
       (if (if (#%app lazy-proc? p)
             '#t
             (#%app struct-constructor-procedure? p))
         (#%app p temp3)
         (#%app p (#%app ! temp3))))
     (#%app ! f)
     (#%app
      lazy
      (lambda ()
        (#%app
         (lambda (p temp4 temp5)
           (if (if (#%app lazy-proc? p)
                 '#t
                 (#%app struct-constructor-procedure? p))
             (#%app p temp4 temp5)
             (#%app p (#%app ! temp4) (#%app ! temp5))))
         (#%app ! +)
         '1
         '10))))) ))