9/20/2010

; program: (! (+ 3 4))

; expanded progs in stepper tests when current-namespace = make-base-namespace

EXPANDED:
    (module stepper-module-name-3991 (lib lazy.ss lazy) (#%plain-module-begin (#%require (for-syntax scheme/mzscheme)) (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) '#t (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) '3 '4))))))
EXPANDED:
    (let-values (((done-already?) '#f)) (#%app dynamic-wind void (lambda () (#%app dynamic-require ''stepper-module-name-3991 '#f)) (lambda () (if done-already? (#%app void) (begin (set! done-already? '#t) (#%app current-namespace (#%app module->namespace ''stepper-module-name-3991)))))))


; expanded progs in stepper tests when current-namespace is not make-base-namespace
EXPANDED:
    (module stepper-module-name-5074 (lib lazy.ss lazy) (#%plain-module-begin (#%require (for-syntax scheme/mzscheme)) (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) '#t (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) '3 '4))))))
EXPANDED:
    (let-values (((done-already?) '#f)) (#%app dynamic-wind void (lambda () (#%app dynamic-require ''stepper-module-name-5074 '#f)) (lambda () (if done-already? (#%app void) (begin (set! done-already? '#t) (#%app current-namespace (#%app module->namespace ''stepper-module-name-5074)))))))
    
; expanded progs in normal stepper    

    EXPANDED:
    (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%ap
p lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%ap
p procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! tem
p2)))) (#%app ! +) (quote 3) (quote 4)))))
    
; with raco expand
    
    (module test lazy
  (#%plain-module-begin
   (#%require (for-syntax scheme/mzscheme))
   (#%app
    !
    (#%app
     lazy
     (lambda ()
       (#%app
        (lambda (p temp1 temp2)
          (if (if (#%app lazy-proc? p)
                '#t
                (#%app struct-constructor-procedure? p))
            (#%app (#%app procedure-extract-target p) temp1 temp2)
            (#%app p (#%app ! temp1) (#%app ! temp2))))
        (#%app ! +)
        '3
        '4))))))
    
    
    
    
    
9/15/2010

left side (pre-unwound):
  (#%plain-app '(lambda (a1) ...) '(make-composable-promise ...))
left side (unwound): ((lambda (a1) ...) (make-composable-promise ...))

RECON VALUE: #<procedure:force>
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f
RECON VALUE: #<promise:...ate\annotate.rkt:809:52>
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f





left side (pre-unwound):
  (#%plain-app force (#%plain-lambda () (#%app (#%plain-lambda (p temp1 temp2) (#%app p temp1 temp2)) + '3 '4)))
left side (unwound): (force (+ 3 4))

RECON VALUE: #<procedure:force>
closure record = #f
RECON VALUE: #<promise:...ate\annotate.rkt:809:52>
closure record = #<closure-record>
RECON VALUE: 3
closure record = #f
RECON VALUE: 4
closure record = #f




RECON VALUE: #<procedure:force>
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f
RECON VALUE: #<promise:...ate\annotate.rkt:809:52>
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f
RECON VALUE: 3
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f
RECON VALUE: 4
RENDER SETTINGS:
true-false-printed? = #t
constructor-style-printing? = #f
abbreviate-cons-as-list? = #t
render-to-sexp = #<procedure:...e\model-settings.rkt:35:45>
lifting? = #f
show-and/or-clauses-consumed? = #t
all-bindings-mutable? = #f






---------- BREAK TYPE = result-exp-break ----------
MARKLIST:
source: (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4))
label: not-yet-called
bindings:
 + : #<procedure:+>
 arg0-13798 : #<*unevaluated-struct*>
 arg1-13799 : #<*unevaluated-struct*>
 arg2-13800 : #<*unevaluated-struct*>
 arg3-13801 : #<*unevaluated-struct*>

source: (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4)))))
label: called
bindings:
 arg0-13798 : #<procedure:force>
 arg1-13799 : #<promise:!running>

source: (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4)))))
label: top-level
bindings:

RETURNED VALUE LIST: #f
held = exps

    ##### reconstruct-all-completed #####
           #####################

checking pending rhs ...
no pending rhs
maybe sending step ... 
left side = (((lambda (a1) ...) (make-composable-promise ...)))
right side = ((... (+ 3 4) ...))
rhs has ellipses
not-equal?: before:
actual:   ((hilite ((lambda (a1) ...) (make-composable-promise ...)))) =/= 
expected: ((hilite (force (+ 3 4))))
  here's the diff: ((hilite (different!-13715 (different!-13715 different!-13715 . different!-13715))))
test-sequence: steps do not match
   given: (before-after-result (((hilite ((lambda (a1) ...) (make-composable-promise ...)))) ((... (hilite (+ 3 4)) ...))))
expected: (before-after ((hilite (force (+ 3 4)))) ((... (hilite (+ 3 4)) ...)))
step sent
last-rhs set
















---------- BREAK TYPE = result-exp-break ----------
MARKLIST:
source: (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4))
label: not-yet-called
bindings:
 + : #<procedure:+>
 arg0-14396 : #<*unevaluated-struct*>
 arg1-14397 : #<*unevaluated-struct*>
 arg2-14398 : #<*unevaluated-struct*>
 arg3-14399 : #<*unevaluated-struct*>

source: (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4)))))
label: called
bindings:
 arg0-14396 : #<procedure:force>
 arg1-14397 : #<promise:!running>

source: (#%app ! (#%app lazy (lambda () (#%app (lambda (p temp1 temp2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-extract-target p) temp1 temp2) (#%app p (#%app ! temp1) (#%app ! temp2)))) (#%app ! +) (quote 3) (quote 4)))))
label: top-level
bindings:

RETURNED VALUE LIST: #f
held = exps

    ##### reconstruct-all-completed #####
           #####################

checking pending rhs ...
no pending rhs
maybe sending step ... 
left side = ((force (+ 3 4)))
right side = ((... (+ 3 4) ...))
rhs has ellipses
step sent
last-rhs set
