#lang scheme

(require "through-tests.ss"
         "test-engine.ss")

(define tests-to-skip
  '(bad-and bad-cons check-error begin-let-bug prims qq-splice time set! local-set!))

(define lazy-tests '(lazy1 lazy2 lazy3 lazy4
                     lazy-top-level-force1 lazy-delay-arg1
                     lazy-parallel-arg-reduce1 lazy-parallel-arg-reduce2 lazy-parallel-arg-reduce3
                     lazy-parallel-arg-reduce-double-apply
                     lazy-multi-value lazy-multi-value-list1
                     lazy-cons1 lazy-cons2 lazy-cons3
                     lazy-list1 lazy-list2 lazy-list3 lazy-list4 lazy-list5 
                     lazy-list6 lazy-list7 lazy-list8 lazy-list9 lazy-list10
                     lazy-list11 lazy-list12 lazy-list13 lazy-list14 lazy-list15
                     lazy-list16 lazy-list17 lazy-list18 lazy-list19 lazy-list20
                     lazy-list21 lazy-list22
                     lazy-list-fn1 lazy-list-fn2 lazy-list-fn3 lazy-list-fn4
                     lazy-list-fn-parallel-reduce1
                     lazy-take1 lazy-take2 lazy-take3 lazy-take4 lazy-take5 lazy-take6 lazy-take7 lazy-take8
                     lazy-list-length))

(let ([ns (make-base-namespace)])
  (namespace-attach-module (current-namespace) 'racket/private/promise ns)
  (parameterize ([display-only-errors #t]
                 [current-output-port (open-output-string)]
                 #;[current-namespace (make-base-namespace)]
                 [current-namespace ns])
    (if (run-tests lazy-tests)
        #;(if (run-all-tests-except (append tests-to-skip lazy-tests))
            (exit 0)
            (exit 1))
        (exit 0)
        (exit 1))))

