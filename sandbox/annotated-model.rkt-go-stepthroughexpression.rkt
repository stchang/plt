exp in annotate/module-top-level: (define-values (f) (#%app lazy-proc (lambda (x
) (#%app lazy (lambda () (let-values (((p) (#%app ! +)) ((temp1) (quote 1)) ((x2
) x)) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedur
e? p)) (#%app p temp1 x2) (#%app p (#%app ! temp1) (#%app ! x2)))))))))
annotated: (begin (define-values (f) (with-continuation-mark #<debug-key-struct>
 (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (#%plai
n-app call-with-values (#%plain-lambda () (let-values (((arg0-1842 arg1-1843) (#
%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-conti
nuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...
rivate/marks.rkt:70:2> (#%plain-lambda () lazy-proc) (#%plain-lambda () lazy) (#
%plain-lambda () +) (#%plain-lambda () arg0-1842) (#%plain-lambda () arg1-1843))
) (begin (set! arg0-1842 (with-continuation-mark #<debug-key-struct> (#%plain-la
mbda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) lazy-proc)) (set! a
rg1-1843 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain
-app #<procedure:...rivate/marks.rkt:70:2>)) (#%plain-app #<procedure:closure-st
oring-proc> (#%plain-lambda (x) (begin (let-values (((arg0-1842 arg1-1843) (#%pl
ain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-continua
tion-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...riv
ate/marks.rkt:70:2> (#%plain-lambda () lazy) (#%plain-lambda () +) (#%plain-lamb
da () x) (#%plain-lambda () arg0-1842) (#%plain-lambda () arg1-1843))) (begin (#
%plain-app #<procedure:result-exp-break>) (begin (set! arg0-1842 (with-continuat
ion-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...riva
te/marks.rkt:70:2>)) lazy)) (set! arg1-1843 (with-continuation-mark #<debug-key-
struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>))
(#%plain-app #<procedure:closure-storing-proc> (#%plain-lambda () (begin (let ((
lifting-counter (#<procedure:binding-indexer>))) (let-values (((lifter-p-0 lifte
r-temp1-1 lifter-x2-2 p temp1 x2 let-counter) (values lifting-counter lifting-co
unter lifting-counter #<*unevaluated-struct*> #<*unevaluated-struct*> #<*unevalu
ated-struct*> 0))) (with-continuation-mark #<debug-key-struct> (#%plain-lambda (
) (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () +) (#%pl
ain-lambda () x) (#%plain-lambda () p) (#%plain-lambda () temp1) (#%plain-lambda
 () x2) (#%plain-lambda () let-counter) (#%plain-lambda () lifter-p-0) (#%plain-
lambda () lifter-temp1-1) (#%plain-lambda () lifter-x2-2))) (begin (#%plain-app
#<procedure:result-exp-break>) (begin (#%plain-app #<procedure:double-break>) (b
egin (set!-values (p) (with-continuation-mark #<debug-key-struct> #<skipto-mark-
struct> (#%app ! (with-continuation-mark #<debug-key-struct> (#%plain-lambda ()
(#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) +)))) (set! let-counter 1)
(set!-values (temp1) (with-continuation-mark #<debug-key-struct> (#%plain-lambda
 () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (quote 1))) (set! let-c
ounter 2) (set!-values (x2) (with-continuation-mark #<debug-key-struct> (#%plain
-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) x)) (set! let-co
unter 3) (#%plain-app #<procedure:exp-finished-break> (#%plain-app list (#%plain
-app list #<procedure:...ate\annotate.rkt:653:69> (#%plain-app list lifter-p-0)
(#%plain-lambda () (#%plain-app list p))) (#%plain-app list #<procedure:...ate\a
nnotate.rkt:653:69> (#%plain-app list lifter-temp1-1) (#%plain-lambda () (#%plai
n-app list temp1))) (#%plain-app list #<procedure:...ate\annotate.rkt:653:69> (#
%plain-app list lifter-x2-2) (#%plain-lambda () (#%plain-app list x2))))) (with-
continuation-mark #<debug-key-struct> #<skipto-mark-struct> (if (if (#%app lazy-
proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (let-values (((arg0
-1842 arg1-1843 arg2-1844) (#%plain-app values #<*unevaluated-struct*> #<*uneval
uated-struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<debug-key-st
ruct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%p
lain-lambda () p) (#%plain-lambda () temp1) (#%plain-lambda () x2) (#%plain-lamb
da () arg0-1842) (#%plain-lambda () arg1-1843) (#%plain-lambda () arg2-1844) (#%
plain-lambda () lifter-p-0) (#%plain-lambda () lifter-temp1-1) (#%plain-lambda (
) lifter-x2-2))) (begin (set! arg0-1842 (with-continuation-mark #<debug-key-stru
ct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (beg
in (#%plain-app #<procedure:normal-break>) (#%plain-app call-with-values (#%plai
n-lambda () p) (#%plain-lambda args (#%plain-app #<procedure:result-value-break>
 args) (#%plain-app #<procedure:apply> values args)))))) (set! arg1-1843 (with-c
ontinuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure
:...rivate/marks.rkt:70:2>)) (begin (#%plain-app #<procedure:normal-break>) (#%p
lain-app call-with-values (#%plain-lambda () temp1) (#%plain-lambda args (#%plai
n-app #<procedure:result-value-break> args) (#%plain-app #<procedure:apply> valu
es args)))))) (set! arg2-1844 (with-continuation-mark #<debug-key-struct> (#%pla
in-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (begin (#%plai
n-app #<procedure:normal-break>) (#%plain-app call-with-values (#%plain-lambda (
) x2) (#%plain-lambda args (#%plain-app #<procedure:result-value-break> args) (#
%plain-app #<procedure:apply> values args)))))) (begin (#%plain-app #<procedure:
normal-break>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#
%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () arg0-1842) (
#%plain-lambda () arg1-1843) (#%plain-lambda () arg2-1844))) (if (#%plain-app #<
procedure:...ivate/shared.rkt:308:7> arg0-1842) (#%plain-app arg0-1842 arg1-1843
 arg2-1844) (#%plain-app call-with-values (#%plain-lambda () (#%plain-app arg0-1
842 arg1-1843 arg2-1844)) (#%plain-lambda args (#%plain-app #<procedure:result-v
alue-break> args) (#%plain-app #<procedure:apply> values args))))))))) (let-valu
es (((arg0-1842 arg1-1843 arg2-1844) (#%plain-app values #<*unevaluated-struct*>
 #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<de
bug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:
70:2> (#%plain-lambda () p) (#%plain-lambda () temp1) (#%plain-lambda () x2) (#%
plain-lambda () arg0-1842) (#%plain-lambda () arg1-1843) (#%plain-lambda () arg2
-1844) (#%plain-lambda () lifter-p-0) (#%plain-lambda () lifter-temp1-1) (#%plai
n-lambda () lifter-x2-2))) (begin (set! arg0-1842 (with-continuation-mark #<debu
g-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70
:2>)) (begin (#%plain-app #<procedure:normal-break>) (#%plain-app call-with-valu
es (#%plain-lambda () p) (#%plain-lambda args (#%plain-app #<procedure:result-va
lue-break> args) (#%plain-app #<procedure:apply> values args)))))) (set! arg1-18
43 (with-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app ! (w
ith-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<proc
edure:...rivate/marks.rkt:70:2>)) (begin (#%plain-app #<procedure:normal-break>)
 (#%plain-app call-with-values (#%plain-lambda () temp1) (#%plain-lambda args (#
%plain-app #<procedure:result-value-break> args) (#%plain-app #<procedure:apply>
 values args)))))))) (set! arg2-1844 (with-continuation-mark #<debug-key-struct>
 #<skipto-mark-struct> (#%app ! (with-continuation-mark #<debug-key-struct> (#%p
lain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (begin (#%pl
ain-app #<procedure:normal-break>) (#%plain-app call-with-values (#%plain-lambda
 () x2) (#%plain-lambda args (#%plain-app #<procedure:result-value-break> args)
(#%plain-app #<procedure:apply> values args)))))))) (begin (#%plain-app #<proced
ure:normal-break>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda (
) (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () arg0-184
2) (#%plain-lambda () arg1-1843) (#%plain-lambda () arg2-1844))) (if (#%plain-ap
p #<procedure:...ivate/shared.rkt:308:7> arg0-1842) (#%plain-app arg0-1842 arg1-
1843 arg2-1844) (#%plain-app call-with-values (#%plain-lambda () (#%plain-app ar
g0-1842 arg1-1843 arg2-1844)) (#%plain-lambda args (#%plain-app #<procedure:resu
lt-value-break> args) (#%plain-app #<procedure:apply> values args)))))))))))))))
)))) (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%pl
ain-lambda () +) (#%plain-lambda () x)))))) (begin (#%plain-app #<procedure:norm
al-break>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%pla
in-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () arg0-1842) (#%pl
ain-lambda () arg1-1843))) (if (#%plain-app #<procedure:...ivate/shared.rkt:308:
7> arg0-1842) (#%plain-app arg0-1842 arg1-1843) (#%plain-app call-with-values (#
%plain-lambda () (#%plain-app arg0-1842 arg1-1843)) (#%plain-lambda args (#%plai
n-app #<procedure:result-value-break> args) (#%plain-app #<procedure:apply> valu
es args)))))))))))) (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.
rkt:70:2> (#%plain-lambda () lazy) (#%plain-lambda () +)))))) (begin (#%plain-ap
p #<procedure:normal-break>) (with-continuation-mark #<debug-key-struct> (#%plai
n-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda (
) arg0-1842) (#%plain-lambda () arg1-1843))) (if (#%plain-app #<procedure:...iva
te/shared.rkt:308:7> arg0-1842) (#%plain-app arg0-1842 arg1-1843) (#%plain-app c
all-with-values (#%plain-lambda () (#%plain-app arg0-1842 arg1-1843)) (#%plain-l
ambda args (#%plain-app #<procedure:result-value-break> args) (#%plain-app #<pro
cedure:apply> values args)))))))))) (#%plain-lambda args (#%plain-app #<procedur
e:apply> values args))))) (#%plain-app #<procedure:exp-finished-break> (#%plain-
app list (#%plain-app list #<procedure:...ate\annotate.rkt:1181:93> #f (#%plain-
lambda () (#%plain-app list f))))))
exp in annotate/module-top-level: (#%app (#%app toplevel-forcer) (let-values (((
p) (#%app ! (#%top . f))) ((temp6) (quote 10))) (if (if (#%app lazy-proc? p) (qu
ote #t) (#%app struct-constructor-procedure? p)) (#%app p temp6) (#%app p (#%app
 ! temp6)))))
exp in annotate/module-top-level: (let-values (((p) (#%app ! (#%top . f))) ((tem
p6) (quote 10))) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-construct
or-procedure? p)) (#%app p temp6) (#%app p (#%app ! temp6))))
annotated: (#%app (#%app toplevel-forcer) (with-continuation-mark #<debug-key-st
ruct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (#
%plain-app call-with-values (#%plain-lambda () (let ((lifting-counter (#<procedu
re:binding-indexer>))) (let-values (((lifter-p-3 lifter-temp6-4 p temp6 let-coun
ter) (values lifting-counter lifting-counter #<*unevaluated-struct*> #<*unevalua
ted-struct*> 0))) (with-continuation-mark #<debug-key-struct> (#%plain-lambda ()
 (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () f) (#%pla
in-lambda () p) (#%plain-lambda () temp6) (#%plain-lambda () let-counter) (#%pla
in-lambda () lifter-p-3) (#%plain-lambda () lifter-temp6-4))) (begin (#%plain-ap
p #<procedure:double-break>) (begin (set!-values (p) (with-continuation-mark #<d
ebug-key-struct> #<skipto-mark-struct> (#%app ! (with-continuation-mark #<debug-
key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2
>)) f)))) (set! let-counter 1) (set!-values (temp6) (with-continuation-mark #<de
bug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:
70:2>)) (quote 10))) (set! let-counter 2) (#%plain-app #<procedure:exp-finished-
break> (#%plain-app list (#%plain-app list #<procedure:...ate\annotate.rkt:653:6
9> (#%plain-app list lifter-p-3) (#%plain-lambda () (#%plain-app list p))) (#%pl
ain-app list #<procedure:...ate\annotate.rkt:653:69> (#%plain-app list lifter-te
mp6-4) (#%plain-lambda () (#%plain-app list temp6))))) (with-continuation-mark #
<debug-key-struct> #<skipto-mark-struct> (if (if (#%app lazy-proc? p) (quote #t)
 (#%app struct-constructor-procedure? p)) (let-values (((arg0-1842 arg1-1843) (#
%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-conti
nuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...
rivate/marks.rkt:70:2> (#%plain-lambda () p) (#%plain-lambda () temp6) (#%plain-
lambda () arg0-1842) (#%plain-lambda () arg1-1843) (#%plain-lambda () lifter-p-3
) (#%plain-lambda () lifter-temp6-4))) (begin (set! arg0-1842 (with-continuation
-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/
marks.rkt:70:2>)) (begin (#%plain-app #<procedure:normal-break>) (#%plain-app ca
ll-with-values (#%plain-lambda () p) (#%plain-lambda args (#%plain-app #<procedu
re:result-value-break> args) (#%plain-app #<procedure:apply> values args)))))) (
set! arg1-1843 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#
%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (begin (#%plain-app #<procedu
re:normal-break>) (#%plain-app call-with-values (#%plain-lambda () temp6) (#%pla
in-lambda args (#%plain-app #<procedure:result-value-break> args) (#%plain-app #
<procedure:apply> values args)))))) (begin (#%plain-app #<procedure:normal-break
>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #
<procedure:...rivate/marks.rkt:70:2> (#%plain-lambda () arg0-1842) (#%plain-lamb
da () arg1-1843))) (if (#%plain-app #<procedure:...ivate/shared.rkt:308:7> arg0-
1842) (#%plain-app arg0-1842 arg1-1843) (#%plain-app call-with-values (#%plain-l
ambda () (#%plain-app arg0-1842 arg1-1843)) (#%plain-lambda args (#%plain-app #<
procedure:result-value-break> args) (#%plain-app #<procedure:apply> values args)
)))))))) (let-values (((arg0-1842 arg1-1843) (#%plain-app values #<*unevaluated-
struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<debug-key-struct>
(#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2> (#%plain-l
ambda () p) (#%plain-lambda () temp6) (#%plain-lambda () arg0-1842) (#%plain-lam
bda () arg1-1843) (#%plain-lambda () lifter-p-3) (#%plain-lambda () lifter-temp6
-4))) (begin (set! arg0-1842 (with-continuation-mark #<debug-key-struct> (#%plai
n-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>)) (begin (#%plain
-app #<procedure:normal-break>) (#%plain-app call-with-values (#%plain-lambda ()
 p) (#%plain-lambda args (#%plain-app #<procedure:result-value-break> args) (#%p
lain-app #<procedure:apply> values args)))))) (set! arg1-1843 (with-continuation
-mark #<debug-key-struct> #<skipto-mark-struct> (#%app ! (with-continuation-mark
 #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks
.rkt:70:2>)) (begin (#%plain-app #<procedure:normal-break>) (#%plain-app call-wi
th-values (#%plain-lambda () temp6) (#%plain-lambda args (#%plain-app #<procedur
e:result-value-break> args) (#%plain-app #<procedure:apply> values args))))))))
(begin (#%plain-app #<procedure:normal-break>) (with-continuation-mark #<debug-k
ey-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate/marks.rkt:70:2>
 (#%plain-lambda () arg0-1842) (#%plain-lambda () arg1-1843))) (if (#%plain-app
#<procedure:...ivate/shared.rkt:308:7> arg0-1842) (#%plain-app arg0-1842 arg1-18
43) (#%plain-app call-with-values (#%plain-lambda () (#%plain-app arg0-1842 arg1
-1843)) (#%plain-lambda args (#%plain-app #<procedure:result-value-break> args)
(#%plain-app #<procedure:apply> values args))))))))))))))))) (#%plain-lambda arg
s (#%plain-app #<procedure:apply> values args)))))
