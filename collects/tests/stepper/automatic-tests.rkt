#lang scheme

(require "through-tests.ss" 
         "test-engine.ss")

(define lazy-tests '(lazy1 lazy2 lazy3 
                     lazy-top-level-force1 lazy-delay-arg1
                     lazy-parallel-arg-reduce1 lazy-parallel-arg-reduce2 lazy-parallel-arg-reduce3
                     lazy-parallel-arg-reduce-double-apply
                     lazy-multi-value
                     lazy-cons1 lazy-list1 lazy-list2
                     ))

(let ([ns (make-base-namespace)])
  (namespace-attach-module (current-namespace) 'racket/private/promise ns)
  (parameterize ([display-only-errors #t]
                 [current-output-port (open-output-string)]
                 #;[current-namespace (make-base-namespace)]
                 [current-namespace ns])
    (if (run-tests lazy-tests)
        #;(if (run-all-tests-except 
             (append
              '(bad-and bad-cons check-error begin-let-bug prims qq-splice time set! local-set!)
              lazy-tests))
            (exit 0)
            (exit 1))
        (exit 0)
        (exit 1))))

