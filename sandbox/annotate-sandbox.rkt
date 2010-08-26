#lang racket
(require (only-in stepper/private/annotate annotate))
;(require (only-in stepper/private/model break))

(define break
  (lambda (mark-set break-kind [returned-value-list #f])
    (void)))

; (annotate main-exp break show-lambdas-as-lambdas? language-level)
(syntax->datum
 (annotate #'(#%plain-app (#%plain-lambda (x) x) (quote 1)) break #f 'testing))



CREATING MARK:
src = x
label= none
bindings:
  x
CREATING MARK:
src = (#%plain-lambda (x) x)
label= none
bindings:
CREATING MARK:
src = (#%plain-lambda (x) x)
label= none
bindings:
CREATING MARK:
src = '1
label= none
bindings:
CREATING MARK:
src = (#%plain-app (#%plain-lambda (x) x) '1)
label= called
bindings:
  arg0-5075
  arg1-5076
CREATING MARK:
src = (#%plain-app (#%plain-lambda (x) x) '1)
label= not-yet-called
bindings:
  arg0-5075
  arg1-5076
CREATING MARK:
src = (#%plain-app (#%plain-lambda (x) x) '1)
label= top-level
bindings:
'(with-continuation-mark
  #<debug-key-struct>
  (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
  (#%plain-app
   call-with-values
   (#%plain-lambda
    ()
    (let-values (((arg0-5075 arg1-5076) (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>)))
      (with-continuation-mark
       #<debug-key-struct>
       (#%plain-lambda
        ()
        (#%plain-app
         #<procedure:...rivate\marks.rkt:70:2>
         (#%plain-lambda () arg0-5075)
         (#%plain-lambda () arg1-5076)))
       (begin
         (set! arg0-5075
           (with-continuation-mark
            #<debug-key-struct>
            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
            (#%plain-app
             #<procedure:closure-storing-proc>
             (#%plain-lambda
              (x)
              (begin
                (with-continuation-mark
                 #<debug-key-struct>
                 (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2> (#%plain-lambda () x)))
                 (begin (#%plain-app #<procedure:result-exp-break>) x))))
             (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)))))
         (set! arg1-5076
           (with-continuation-mark
            #<debug-key-struct>
            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
            '1))
         (begin
           (#%plain-app #<procedure:normal-break>)
           (with-continuation-mark
            #<debug-key-struct>
            (#%plain-lambda
             ()
             (#%plain-app
              #<procedure:...rivate\marks.rkt:70:2>
              (#%plain-lambda () arg0-5075)
              (#%plain-lambda () arg1-5076)))
            (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-5075)
              (#%plain-app arg0-5075 arg1-5076)
              (#%plain-app
               call-with-values
               (#%plain-lambda () (#%plain-app arg0-5075 arg1-5076))
               (#%plain-lambda
                args
                (#%plain-app #<procedure:result-value-break> args)
                (#%plain-app #<procedure:apply> values args))))))))))
   (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))









; (annotate #'(#%plain-app (#%plain-lambda (x) (#%plain-app (quote 1) x)) (#%plain-app + (quote 7) (quote 8))) break #f 'testing))
;
;
;
;CREATING MARK:
;src = '1
;label= none
;bindings:
;CREATING MARK:
;src = x
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-app '1 x)
;label= called
;bindings:
;  arg0-3593
;  arg1-3594
;CREATING MARK:
;src = (#%plain-app '1 x)
;label= not-yet-called
;bindings:
;  x
;  arg0-3593
;  arg1-3594
;CREATING MARK:
;src = (#%plain-lambda (x) (#%plain-app '1 x))
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-lambda (x) (#%plain-app '1 x))
;label= none
;bindings:
;CREATING MARK:
;src = +
;label= none
;bindings:
;CREATING MARK:
;src = '7
;label= none
;bindings:
;CREATING MARK:
;src = '8
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= called
;bindings:
;  arg0-3593
;  arg1-3594
;  arg2-3595
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= not-yet-called
;bindings:
;  arg0-3593
;  arg1-3594
;  arg2-3595
;CREATING MARK:
;src = (#%plain-app (#%plain-lambda (x) (#%plain-app '1 x)) (#%plain-app + '7 '8))
;label= called
;bindings:
;  arg0-3593
;  arg1-3594
;CREATING MARK:
;src = (#%plain-app (#%plain-lambda (x) (#%plain-app '1 x)) (#%plain-app + '7 '8))
;label= not-yet-called
;bindings:
;  +
;  arg0-3593
;  arg1-3594
;CREATING MARK:
;src = (#%plain-app (#%plain-lambda (x) (#%plain-app '1 x)) (#%plain-app + '7 '8))
;label= top-level
;bindings:
;
;
;
;'(with-continuation-mark
;  #<debug-key-struct>
;  (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;  (#%plain-app
;   call-with-values
;   (#%plain-lambda
;    ()
;    (let-values (((arg0-3593 arg1-3594) (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>)))
;      (with-continuation-mark
;       #<debug-key-struct>
;       (#%plain-lambda
;        ()
;        (#%plain-app
;         #<procedure:...rivate\marks.rkt:70:2>
;         (#%plain-lambda () +)
;         (#%plain-lambda () arg0-3593)
;         (#%plain-lambda () arg1-3594)))
;       (begin
;         (set! arg0-3593
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            (#%plain-app
;             #<procedure:closure-storing-proc>
;             (#%plain-lambda
;              (x)
;              (begin
;                (let-values (((arg0-3593 arg1-3594)
;                              (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>)))
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda
;                    ()
;                    (#%plain-app
;                     #<procedure:...rivate\marks.rkt:70:2>
;                     (#%plain-lambda () x)
;                     (#%plain-lambda () arg0-3593)
;                     (#%plain-lambda () arg1-3594)))
;                   (begin
;                     (#%plain-app #<procedure:result-exp-break>)
;                     (begin
;                       (set! arg0-3593
;                         (with-continuation-mark
;                          #<debug-key-struct>
;                          (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                          '1))
;                       (set! arg1-3594
;                         (with-continuation-mark
;                          #<debug-key-struct>
;                          (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                          x))
;                       (begin
;                         (#%plain-app #<procedure:normal-break>)
;                         (with-continuation-mark
;                          #<debug-key-struct>
;                          (#%plain-lambda
;                           ()
;                           (#%plain-app
;                            #<procedure:...rivate\marks.rkt:70:2>
;                            (#%plain-lambda () arg0-3593)
;                            (#%plain-lambda () arg1-3594)))
;                          (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-3593)
;                            (#%plain-app arg0-3593 arg1-3594)
;                            (#%plain-app
;                             call-with-values
;                             (#%plain-lambda () (#%plain-app arg0-3593 arg1-3594))
;                             (#%plain-lambda
;                              args
;                              (#%plain-app #<procedure:result-value-break> args)
;                              (#%plain-app #<procedure:apply> values args))))))))))))
;             (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>)))))
;         (set! arg1-3594
;           (let-values (((arg0-3593 arg1-3594 arg2-3595)
;                         (#%plain-app
;                          values
;                          #<*unevaluated-struct*>
;                          #<*unevaluated-struct*>
;                          #<*unevaluated-struct*>)))
;             (with-continuation-mark
;              #<debug-key-struct>
;              (#%plain-lambda
;               ()
;               (#%plain-app
;                #<procedure:...rivate\marks.rkt:70:2>
;                (#%plain-lambda () arg0-3593)
;                (#%plain-lambda () arg1-3594)
;                (#%plain-lambda () arg2-3595)))
;              (begin
;                (set! arg0-3593
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   +))
;                (set! arg1-3594
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   '7))
;                (set! arg2-3595
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   '8))
;                (begin
;                  (#%plain-app #<procedure:normal-break>)
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda
;                    ()
;                    (#%plain-app
;                     #<procedure:...rivate\marks.rkt:70:2>
;                     (#%plain-lambda () arg0-3593)
;                     (#%plain-lambda () arg1-3594)
;                     (#%plain-lambda () arg2-3595)))
;                   (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-3593)
;                     (#%plain-app arg0-3593 arg1-3594 arg2-3595)
;                     (#%plain-app
;                      call-with-values
;                      (#%plain-lambda () (#%plain-app arg0-3593 arg1-3594 arg2-3595))
;                      (#%plain-lambda
;                       args
;                       (#%plain-app #<procedure:result-value-break> args)
;                       (#%plain-app #<procedure:apply> values args))))))))))
;         (begin
;           (#%plain-app #<procedure:normal-break>)
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda
;             ()
;             (#%plain-app
;              #<procedure:...rivate\marks.rkt:70:2>
;              (#%plain-lambda () arg0-3593)
;              (#%plain-lambda () arg1-3594)))
;            (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-3593)
;              (#%plain-app arg0-3593 arg1-3594)
;              (#%plain-app
;               call-with-values
;               (#%plain-lambda () (#%plain-app arg0-3593 arg1-3594))
;               (#%plain-lambda
;                args
;                (#%plain-app #<procedure:result-value-break> args)
;                (#%plain-app #<procedure:apply> values args))))))))))
;   (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))









;(syntax->datum
; (annotate #'(#%plain-app + (quote 4) (#%plain-app + (quote 7) (quote 8))) break #f 'testing))
;
;CREATING MARK:
;src = +
;label= none
;bindings:
;CREATING MARK:
;src = '4
;label= none
;bindings:
;CREATING MARK:
;src = +
;label= none
;bindings:
;CREATING MARK:
;src = '7
;label= none
;bindings:
;CREATING MARK:
;src = '8
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= called
;bindings:
;  arg0-3080
;  arg1-3081
;  arg2-3082
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= not-yet-called
;bindings:
;  arg0-3080
;  arg1-3081
;  arg2-3082
;CREATING MARK:
;src = (#%plain-app + '4 (#%plain-app + '7 '8))
;label= called
;bindings:
;  arg0-3080
;  arg1-3081
;  arg2-3082
;CREATING MARK:
;src = (#%plain-app + '4 (#%plain-app + '7 '8))
;label= not-yet-called
;bindings:
;  +
;  arg0-3080
;  arg1-3081
;  arg2-3082
;CREATING MARK:
;src = (#%plain-app + '4 (#%plain-app + '7 '8))
;label= top-level
;bindings:
;
;
;
;
;
;'(with-continuation-mark
;  #<debug-key-struct>
;  (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;  (#%plain-app
;   call-with-values
;   (#%plain-lambda
;    ()
;    (let-values (((arg0-3080 arg1-3081 arg2-3082)
;                  (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*> #<*unevaluated-struct*>)))
;      (with-continuation-mark
;       #<debug-key-struct>
;       (#%plain-lambda
;        ()
;        (#%plain-app
;         #<procedure:...rivate\marks.rkt:70:2>
;         (#%plain-lambda () +)
;         (#%plain-lambda () arg0-3080)
;         (#%plain-lambda () arg1-3081)
;         (#%plain-lambda () arg2-3082)))
;       (begin
;         (set! arg0-3080
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            +))
;         (set! arg1-3081
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            '4))
;         (set! arg2-3082
;           (let-values (((arg0-3080 arg1-3081 arg2-3082)
;                         (#%plain-app
;                          values
;                          #<*unevaluated-struct*>
;                          #<*unevaluated-struct*>
;                          #<*unevaluated-struct*>)))
;             (with-continuation-mark
;              #<debug-key-struct>
;              (#%plain-lambda
;               ()
;               (#%plain-app
;                #<procedure:...rivate\marks.rkt:70:2>
;                (#%plain-lambda () arg0-3080)
;                (#%plain-lambda () arg1-3081)
;                (#%plain-lambda () arg2-3082)))
;              (begin
;                (set! arg0-3080
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   +))
;                (set! arg1-3081
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   '7))
;                (set! arg2-3082
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;                   '8))
;                (begin
;                  (#%plain-app #<procedure:normal-break>)
;                  (with-continuation-mark
;                   #<debug-key-struct>
;                   (#%plain-lambda
;                    ()
;                    (#%plain-app
;                     #<procedure:...rivate\marks.rkt:70:2>
;                     (#%plain-lambda () arg0-3080)
;                     (#%plain-lambda () arg1-3081)
;                     (#%plain-lambda () arg2-3082)))
;                   (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-3080)
;                     (#%plain-app arg0-3080 arg1-3081 arg2-3082)
;                     (#%plain-app
;                      call-with-values
;                      (#%plain-lambda () (#%plain-app arg0-3080 arg1-3081 arg2-3082))
;                      (#%plain-lambda
;                       args
;                       (#%plain-app #<procedure:result-value-break> args)
;                       (#%plain-app #<procedure:apply> values args))))))))))
;         (begin
;           (#%plain-app #<procedure:normal-break>)
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda
;             ()
;             (#%plain-app
;              #<procedure:...rivate\marks.rkt:70:2>
;              (#%plain-lambda () arg0-3080)
;              (#%plain-lambda () arg1-3081)
;              (#%plain-lambda () arg2-3082)))
;            (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-3080)
;              (#%plain-app arg0-3080 arg1-3081 arg2-3082)
;              (#%plain-app
;               call-with-values
;               (#%plain-lambda () (#%plain-app arg0-3080 arg1-3081 arg2-3082))
;               (#%plain-lambda
;                args
;                (#%plain-app #<procedure:result-value-break> args)
;                (#%plain-app #<procedure:apply> values args))))))))))
;   (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))





;(syntax->datum
; (annotate #'(#%plain-app + (quote 7) (quote 8)) break #f 'testing))
;
;
;CREATING MARK:
;src = +
;label= none
;bindings:
;CREATING MARK:
;src = '7
;label= none
;bindings:
;CREATING MARK:
;src = '8
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= called
;bindings:
;  arg0-2567
;  arg1-2568
;  arg2-2569
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= not-yet-called
;bindings:
;  +
;  arg0-2567
;  arg1-2568
;  arg2-2569
;CREATING MARK:
;src = (#%plain-app + '7 '8)
;label= top-level
;bindings:
;
;
;
;'(with-continuation-mark
;  #<debug-key-struct>
;  (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;  (#%plain-app
;   call-with-values
;   (#%plain-lambda
;    ()
;    (let-values (((arg0-2567 arg1-2568 arg2-2569)
;                  (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*> #<*unevaluated-struct*>)))
;      (with-continuation-mark
;       #<debug-key-struct>
;       (#%plain-lambda
;        ()
;        (#%plain-app
;         #<procedure:...rivate\marks.rkt:70:2>
;         (#%plain-lambda () +)
;         (#%plain-lambda () arg0-2567)
;         (#%plain-lambda () arg1-2568)
;         (#%plain-lambda () arg2-2569)))
;       (begin
;         (set! arg0-2567
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            +))
;         (set! arg1-2568
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            '7))
;         (set! arg2-2569
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            '8))
;         (begin
;           (#%plain-app #<procedure:normal-break>)
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda
;             ()
;             (#%plain-app
;              #<procedure:...rivate\marks.rkt:70:2>
;              (#%plain-lambda () arg0-2567)
;              (#%plain-lambda () arg1-2568)
;              (#%plain-lambda () arg2-2569)))
;            (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-2567)
;              (#%plain-app arg0-2567 arg1-2568 arg2-2569)
;              (#%plain-app
;               call-with-values
;               (#%plain-lambda () (#%plain-app arg0-2567 arg1-2568 arg2-2569))
;               (#%plain-lambda
;                args
;                (#%plain-app #<procedure:result-value-break> args)
;                (#%plain-app #<procedure:apply> values args))))))))))
;   (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))







#;(syntax->datum
 (annotate #'(#%plain-app test (quote 1)) break #f 'testing))
;CREATING MARK:
;src = test
;label= none
;bindings:
;CREATING MARK:
;src = '1
;label= none
;bindings:
;CREATING MARK:
;src = (#%plain-app test '1)
;label= called
;bindings:
;  arg0-2037
;  arg1-2038
;CREATING MARK:
;src = (#%plain-app test '1)
;label= not-yet-called
;bindings:
;  test
;  arg0-2037
;  arg1-2038
;CREATING MARK:
;src = (#%plain-app test '1)
;label= top-level
;bindings:
;
;'(with-continuation-mark
;  #<debug-key-struct>
;  (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;  (#%plain-app
;   call-with-values
;   (#%plain-lambda
;    ()
;    (let-values (((arg0-2037 arg1-2038) (#%plain-app values #<*unevaluated-struct*> #<*unevaluated-struct*>)))
;      (with-continuation-mark
;       #<debug-key-struct>
;       (#%plain-lambda
;        ()
;        (#%plain-app
;         #<procedure:...rivate\marks.rkt:70:2>
;         (#%plain-lambda () test)
;         (#%plain-lambda () arg0-2037)
;         (#%plain-lambda () arg1-2038)))
;       (begin
;         (set! arg0-2037
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            test))
;         (set! arg1-2038
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda () (#%plain-app #<procedure:...rivate\marks.rkt:70:2>))
;            '1))
;         (begin
;           (#%plain-app #<procedure:normal-break>)
;           (with-continuation-mark
;            #<debug-key-struct>
;            (#%plain-lambda
;             ()
;             (#%plain-app
;              #<procedure:...rivate\marks.rkt:70:2>
;              (#%plain-lambda () arg0-2037)
;              (#%plain-lambda () arg1-2038)))
;            (if (#%plain-app #<procedure:...ivate\shared.rkt:308:7> arg0-2037)
;              (#%plain-app arg0-2037 arg1-2038)
;              (#%plain-app
;               call-with-values
;               (#%plain-lambda () (#%plain-app arg0-2037 arg1-2038))
;               (#%plain-lambda
;                args
;                (#%plain-app #<procedure:result-value-break> args)
;                (#%plain-app #<procedure:apply> values args))))))))))
;   (#%plain-lambda args (#%plain-app #<procedure:apply> values args))))