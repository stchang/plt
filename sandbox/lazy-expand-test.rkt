;(module test lazy
 (define (f x) (+ x x))
(f (+ 1 10))
;)

; (define (f x) (+ 1 x))
; (f 10)
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