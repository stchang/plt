dumps are from the following test program in lazy racket:

(define (f x) (+ 1 x))
(f 10)

- trying to fix problem where define functions were not displaying properly
- closure-record not found (in reconstruct.rkt::recon-value) when lambdas were wrapped with lazy-proc struct constructor

- turns out lazy-proc wrapped lambdas printed the same as non-wrapped lambdas so I thought that all lambdas were not getting wrapped
- fixed the problem of closure-records not being found in closure table but first extracting lambda from lazy-proc struct using procedure-extract-target