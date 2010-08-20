;UNANNOTATED:
  (define-values (f) (#%app lazy-proc (lambda (x) (#%app lazy (lambda () (#%app
(lambda (p x1 x2) (if (if (#%app lazy-proc? p) (quote #t) (#%app struct-construc
tor-procedure? p)) (#%app (#%app procedure-extract-target p) x1 x2) (#%app p (#%
app ! x1) (#%app ! x2)))) (#%app ! +) x x))))))
;ANNOTATED:
  (begin (define-values (f) (with-continuation-mark #<debug-key-struct> (#%plain
-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) (#%plain-app cal
l-with-values (#%plain-lambda () (with-continuation-mark #<debug-key-struct> #<s
kipto-mark-struct> (#%app lazy-proc (with-continuation-mark #<debug-key-struct>
(#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-l
ambda () +))) (#%plain-app #<procedure:closure-storing-proc> (#%plain-lambda (x)
 (begin (with-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app
 lazy (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-ap
p #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () +) (#%plain-lambda ()
 x))) (begin (#%plain-app #<procedure:result-exp-break>) (#%plain-app #<procedur
e:closure-storing-proc> (#%plain-lambda () (begin (let-values (((arg0-1892 arg1-
1893 arg2-1894 arg3-1895) (#%plain-app values #<*unevaluated-struct*> #<*unevalu
ated-struct*> #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-continuat
ion-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...riva
te\marks.rkt:70:2> (#%plain-lambda () +) (#%plain-lambda () x) (#%plain-lambda (
) arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-lambda () arg2-1894) (#%plai
n-lambda () arg3-1895))) (begin (#%plain-app #<procedure:result-exp-break>) (beg
in (set! arg0-1892 (with-continuation-mark #<debug-key-struct> (#%plain-lambda (
) (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) (#%plain-app #<procedure:
closure-storing-proc> (#%plain-lambda (p x1 x2) (begin (with-continuation-mark #
<debug-key-struct> #<skipto-mark-struct> (if (if (#%app lazy-proc? p) (quote #t)
 (#%app struct-constructor-procedure? p)) (let-values (((arg0-1892 arg1-1893 arg
2-1894) (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*> #<*u
nevaluated-struct*>))) (with-continuation-mark #<debug-key-struct> (#%plain-lamb
da () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () p) (
#%plain-lambda () x1) (#%plain-lambda () x2) (#%plain-lambda () arg0-1892) (#%pl
ain-lambda () arg1-1893) (#%plain-lambda () arg2-1894))) (begin (#%plain-app #<p
rocedure:result-exp-break>) (begin (set! arg0-1892 (with-continuation-mark #<deb
ug-key-struct> #<skipto-mark-struct> (#%app procedure-extract-target (with-conti
nuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...
rivate\marks.rkt:70:2>)) p)))) (set! arg1-1893 (with-continuation-mark #<debug-k
ey-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>
)) x1)) (set! arg2-1894 (with-continuation-mark #<debug-key-struct> (#%plain-lam
bda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) x2)) (begin (#%plain
-app #<procedure:normal-break>) (with-continuation-mark #<debug-key-struct> (#%p
lain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambd
a () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-lambda () arg2-1894))) (i
f (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-1892) (#%plain-app ar
g0-1892 arg1-1893 arg2-1894) (#%plain-app call-with-values (#%plain-lambda () (#
%plain-app arg0-1892 arg1-1893 arg2-1894)) (#%plain-lambda args (#%plain-app #<p
rocedure:result-value-break> args) (#%plain-app #<procedure:apply> values args))
)))))))) (let-values (((arg0-1892 arg1-1893 arg2-1894) (#%plain-app values #<*un
evaluated-struct*> #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-cont
inuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:..
.rivate\marks.rkt:70:2> (#%plain-lambda () p) (#%plain-lambda () x1) (#%plain-la
mbda () x2) (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain
-lambda () arg2-1894))) (begin (#%plain-app #<procedure:result-exp-break>) (begi
n (set! arg0-1892 (with-continuation-mark #<debug-key-struct> (#%plain-lambda ()
 (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) p)) (set! arg1-1893 (with-
continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app ! (with-conti
nuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...
rivate\marks.rkt:70:2>)) x1)))) (set! arg2-1894 (with-continuation-mark #<debug-
key-struct> #<skipto-mark-struct> (#%app ! (with-continuation-mark #<debug-key-s
truct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) x
2)))) (begin (#%plain-app #<procedure:normal-break>) (with-continuation-mark #<d
ebug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt
:70:2> (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-lamb
da () arg2-1894))) (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-
1892) (#%plain-app arg0-1892 arg1-1893 arg2-1894) (#%plain-app call-with-values
(#%plain-lambda () (#%plain-app arg0-1892 arg1-1893 arg2-1894)) (#%plain-lambda
args (#%plain-app #<procedure:result-value-break> args) (#%plain-app #<procedure
:apply> values args)))))))))))))) (#%plain-lambda () (#%plain-app #<procedure:..
.rivate\marks.rkt:70:2>))))) (set! arg1-1893 (with-continuation-mark #<debug-key
-struct> #<skipto-mark-struct> (#%app ! (with-continuation-mark #<debug-key-stru
ct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) +)))
) (set! arg2-1894 (with-continuation-mark #<debug-key-struct> (#%plain-lambda ()
 (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) x)) (set! arg3-1895 (with-
continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedur
e:...rivate\marks.rkt:70:2>)) x)) (begin (#%plain-app #<procedure:normal-break>)
 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<p
rocedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892) (#%plain-lambda
 () arg1-1893) (#%plain-lambda () arg2-1894) (#%plain-lambda () arg3-1895))) (if
 (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-1892) (#%plain-app arg
0-1892 arg1-1893 arg2-1894 arg3-1895) (#%plain-app call-with-values (#%plain-lam
bda () (#%plain-app arg0-1892 arg1-1893 arg2-1894 arg3-1895)) (#%plain-lambda ar
gs (#%plain-app #<procedure:result-value-break> args) (#%plain-app #<procedure:a
pply> values args)))))))))))) (#%plain-lambda () (#%plain-app #<procedure:...riv
ate\marks.rkt:70:2> (#%plain-lambda () +) (#%plain-lambda () x)))))))))) (#%plai
n-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda (
) +)))))))) (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))
) (#%plain-app #<procedure:exp-finished-break> (#%plain-app list (#%plain-app li
st #<procedure:...ate\annotate.rkt:1200:93> #f (#%plain-lambda () (#%plain-app l
ist f))))))

;UNANNOTATED:
  (#%app (#%app toplevel-forcer) (#%app (lambda (p temp6) (if (if (#%app lazy-pr
oc? p) (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedu
re-extract-target p) temp6) (#%app p (#%app ! temp6)))) (#%app ! (#%top . f)) (#
%app lazy (lambda () (#%app (lambda (p temp7 temp8) (if (if (#%app lazy-proc? p)
 (quote #t) (#%app struct-constructor-procedure? p)) (#%app (#%app procedure-ext
ract-target p) temp7 temp8) (#%app p (#%app ! temp7) (#%app ! temp8)))) (#%app !
 +) (quote 1) (quote 10))))))
;ANNOTATED:
  (#%app (#%app toplevel-forcer) (with-continuation-mark #<debug-key-struct> (#%
plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) (#%plain-ap
p call-with-values (#%plain-lambda () (let-values (((arg0-1892 arg1-1893 arg2-18
94) (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*> #<*uneva
luated-struct*>))) (with-continuation-mark #<debug-key-struct> (#%plain-lambda (
) (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () f) (#%pl
ain-lambda () +) (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%
plain-lambda () arg2-1894))) (begin (set! arg0-1892 (with-continuation-mark #<de
bug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:
70:2>)) (#%plain-app #<procedure:closure-storing-proc> (#%plain-lambda (p temp6)
 (begin (with-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (if (i
f (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p)) (let-
values (((arg0-1892 arg1-1893) (#%plain-app values #<*unevaluated-struct*> #<*un
evaluated-struct*>))) (with-continuation-mark #<debug-key-struct> (#%plain-lambd
a () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () p) (#
%plain-lambda () temp6) (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-18
93))) (begin (#%plain-app #<procedure:result-exp-break>) (begin (set! arg0-1892
(with-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app procedu
re-extract-target (with-continuation-mark #<debug-key-struct> (#%plain-lambda ()
 (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) p)))) (set! arg1-1893 (wit
h-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<proced
ure:...rivate\marks.rkt:70:2>)) temp6)) (begin (#%plain-app #<procedure:normal-b
reak>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-a
pp #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892) (#%plain-
lambda () arg1-1893))) (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> a
rg0-1892) (#%plain-app arg0-1892 arg1-1893) (#%plain-app call-with-values (#%pla
in-lambda () (#%plain-app arg0-1892 arg1-1893)) (#%plain-lambda args (#%plain-ap
p #<procedure:result-value-break> args) (#%plain-app #<procedure:apply> values a
rgs)))))))))) (let-values (((arg0-1892 arg1-1893) (#%plain-app values #<*unevalu
ated-struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<debug-key-str
uct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%pl
ain-lambda () p) (#%plain-lambda () temp6) (#%plain-lambda () arg0-1892) (#%plai
n-lambda () arg1-1893))) (begin (#%plain-app #<procedure:result-exp-break>) (beg
in (set! arg0-1892 (with-continuation-mark #<debug-key-struct> (#%plain-lambda (
) (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) p)) (set! arg1-1893 (with
-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app ! (with-cont
inuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:..
.rivate\marks.rkt:70:2>)) temp6)))) (begin (#%plain-app #<procedure:normal-break
>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #
<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892) (#%plain-lamb
da () arg1-1893))) (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-
1892) (#%plain-app arg0-1892 arg1-1893) (#%plain-app call-with-values (#%plain-l
ambda () (#%plain-app arg0-1892 arg1-1893)) (#%plain-lambda args (#%plain-app #<
procedure:result-value-break> args) (#%plain-app #<procedure:apply> values args)
))))))))))))) (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70
:2>))))) (set! arg1-1893 (with-continuation-mark #<debug-key-struct> #<skipto-ma
rk-struct> (#%app ! (with-continuation-mark #<debug-key-struct> (#%plain-lambda
() (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) f)))) (set! arg2-1894 (w
ith-continuation-mark #<debug-key-struct> #<skipto-mark-struct> (#%app lazy (wit
h-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<proced
ure:...rivate\marks.rkt:70:2>)) (#%plain-app #<procedure:closure-storing-proc> (
#%plain-lambda () (begin (let-values (((arg0-1892 arg1-1893 arg2-1894 arg3-1895)
 (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*> #<*unevalua
ted-struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<debug-key-stru
ct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%pla
in-lambda () +) (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%p
lain-lambda () arg2-1894) (#%plain-lambda () arg3-1895))) (begin (#%plain-app #<
procedure:result-exp-break>) (begin (set! arg0-1892 (with-continuation-mark #<de
bug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:
70:2>)) (#%plain-app #<procedure:closure-storing-proc> (#%plain-lambda (p temp7
temp8) (begin (with-continuation-mark #<debug-key-struct> #<skipto-mark-struct>
(if (if (#%app lazy-proc? p) (quote #t) (#%app struct-constructor-procedure? p))
 (let-values (((arg0-1892 arg1-1893 arg2-1894) (#%plain-app values #<*unevaluate
d-struct*> #<*unevaluated-struct*> #<*unevaluated-struct*>))) (with-continuation
-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\
marks.rkt:70:2> (#%plain-lambda () p) (#%plain-lambda () temp7) (#%plain-lambda
() temp8) (#%plain-lambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-l
ambda () arg2-1894))) (begin (#%plain-app #<procedure:result-exp-break>) (begin
(set! arg0-1892 (with-continuation-mark #<debug-key-struct> #<skipto-mark-struct
> (#%app procedure-extract-target (with-continuation-mark #<debug-key-struct> (#
%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) p)))) (set
! arg1-1893 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%pl
ain-app #<procedure:...rivate\marks.rkt:70:2>)) temp7)) (set! arg2-1894 (with-co
ntinuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:
...rivate\marks.rkt:70:2>)) temp8)) (begin (#%plain-app #<procedure:normal-break
>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #
<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892) (#%plain-lamb
da () arg1-1893) (#%plain-lambda () arg2-1894))) (if (#%plain-app #<procedure:..
.ivate\shared.rkt:308:7> arg0-1892) (#%plain-app arg0-1892 arg1-1893 arg2-1894)
(#%plain-app call-with-values (#%plain-lambda () (#%plain-app arg0-1892 arg1-189
3 arg2-1894)) (#%plain-lambda args (#%plain-app #<procedure:result-value-break>
args) (#%plain-app #<procedure:apply> values args)))))))))) (let-values (((arg0-
1892 arg1-1893 arg2-1894) (#%plain-app values #<*unevaluated-struct*> #<*unevalu
ated-struct*> #<*unevaluated-struct*>))) (with-continuation-mark #<debug-key-str
uct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%pl
ain-lambda () p) (#%plain-lambda () temp7) (#%plain-lambda () temp8) (#%plain-la
mbda () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-lambda () arg2-1894)))
 (begin (#%plain-app #<procedure:result-exp-break>) (begin (set! arg0-1892 (with
-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedu
re:...rivate\marks.rkt:70:2>)) p)) (set! arg1-1893 (with-continuation-mark #<deb
ug-key-struct> #<skipto-mark-struct> (#%app ! (with-continuation-mark #<debug-ke
y-struct> (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)
) temp7)))) (set! arg2-1894 (with-continuation-mark #<debug-key-struct> #<skipto
-mark-struct> (#%app ! (with-continuation-mark #<debug-key-struct> (#%plain-lamb
da () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) temp8)))) (begin (#%p
lain-app #<procedure:normal-break>) (with-continuation-mark #<debug-key-struct>
(#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-l
ambda () arg0-1892) (#%plain-lambda () arg1-1893) (#%plain-lambda () arg2-1894))
) (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-1892) (#%plain-ap
p arg0-1892 arg1-1893 arg2-1894) (#%plain-app call-with-values (#%plain-lambda (
) (#%plain-app arg0-1892 arg1-1893 arg2-1894)) (#%plain-lambda args (#%plain-app
 #<procedure:result-value-break> args) (#%plain-app #<procedure:apply> values ar
gs)))))))))))))) (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt
:70:2>))))) (set! arg1-1893 (with-continuation-mark #<debug-key-struct> #<skipto
-mark-struct> (#%app ! (with-continuation-mark #<debug-key-struct> (#%plain-lamb
da () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)) +)))) (set! arg2-1894
 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<p
rocedure:...rivate\marks.rkt:70:2>)) (quote 1))) (set! arg3-1895 (with-continuat
ion-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<procedure:...riva
te\marks.rkt:70:2>)) (quote 10))) (begin (#%plain-app #<procedure:normal-break>)
 (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (#%plain-app #<p
rocedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892) (#%plain-lambda
 () arg1-1893) (#%plain-lambda () arg2-1894) (#%plain-lambda () arg3-1895))) (if
 (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-1892) (#%plain-app arg
0-1892 arg1-1893 arg2-1894 arg3-1895) (#%plain-app call-with-values (#%plain-lam
bda () (#%plain-app arg0-1892 arg1-1893 arg2-1894 arg3-1895)) (#%plain-lambda ar
gs (#%plain-app #<procedure:result-value-break> args) (#%plain-app #<procedure:a
pply> values args)))))))))))) (#%plain-lambda () (#%plain-app #<procedure:...riv
ate\marks.rkt:70:2> (#%plain-lambda () +)))))))) (begin (#%plain-app #<procedure
:normal-break>) (with-continuation-mark #<debug-key-struct> (#%plain-lambda () (
#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () arg0-1892)
(#%plain-lambda () arg1-1893) (#%plain-lambda () arg2-1894))) (if (#%plain-app #
<procedure:...ivate\shared.rkt:308:7> arg0-1892) (#%plain-app arg0-1892 arg1-189
3 arg2-1894) (#%plain-app call-with-values (#%plain-lambda () (#%plain-app arg0-
1892 arg1-1893 arg2-1894)) (#%plain-lambda args (#%plain-app #<procedure:result-
value-break> args) (#%plain-app #<procedure:apply> values args)))))))))) (#%plai
n-lambda args (#%plain-app #<procedure:apply> values args)))))