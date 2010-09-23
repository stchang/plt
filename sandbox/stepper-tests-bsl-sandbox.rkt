;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stepper-tests-bsl-sandbox) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;(define-struct mamba (rhythm tempo)) 
;(mamba-rhythm (make-mamba 24 2))
; fixed by checking for stepper-define-struct-hint property before printing getter in exp-finished-break (in model.rkt::go::break)

;(let ([a (lambda (x) (+ x 5))]) (a 6))
; fixed by:
; 1) wrapping let-clauses in annotate/inner::let-abstraction in thunks
; 2) not using eval in reconstruct::recon-inner::maybe-extract-binding

;(cons 3 (cons 1 empty)) 
; fixed by reconstructing empty list specially in recon-value

(check-expect (+ 3 4) (+ 8 9))
(check-expect (+ 1 1) 2)
(check-expect (+ 2 2) 4) 
(+ 4 5)


;test-sequence: steps do not match
;   given: #(struct:error-result "printf: expects argument of type <pattern-string (tag `~1' not allowed)>; given \" In C:\\\\DOCUME~1\\\\STEPHE~1\\\\LOCALS~1\\\\Temp\\\\stepper-test at line (unknown) column (unknown)\"")
;expected: (finished-stepping)
;...Error has occurred during test: check-expect