#lang scheme/base

;step collector state machine (not yet implemented):
;
; datatype held-type = NO-HELD-STEP | SKIPPED-STEP | HELD(args)
;
; states: global state of held
; global: held : held-type
; edge-names: first, skipped-first, second, skipped-second, double, late-let
;
;transitions (& actions):
;
; held = NO-HELD-STEP :
;  first(x) : held := HELD(x)
;  skipped-first : held := SKIPPED-STEP
;  second(x) : trigger(NO-HELD-STEP, x), held := NO-HELD-STEP.
;      this happens when evaluating unannotated code
;  skipped-second : held := NO-HELD-STEP
;      I believe this can also arise in unannotated code
;  double(x) : double-trigger(x), held := NO-HELD-STEP
;  late-let(x) : late-let-trigger(x), held := NO-HELD-STEP
;
; held = SOME(SKIPPED-STEP) :
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : held := NO-HELD-STEP
;      this happens e.g. for evaluation of top-level var bound to a procedure
;  skipped-second : held := NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR
;
; held = SOME(HELD(args))
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : trigger(HELD(args),x), held = NO-HELD-STEP
;  skipped-second : held = NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR


(require scheme/contract
         scheme/match
         scheme/class
         scheme/list
         (prefix-in a: "annotate.ss")
         (prefix-in r: "reconstruct.ss")
         "shared.ss"
         "marks.ss"
         "model-settings.ss"
         "macro-unwind.ss"
         "lifting.ss"
         (prefix-in test-engine: test-engine/scheme-tests)
         #;(file "/Users/clements/clements/scheme-scraps/eli-debug.ss")
         ;; for breakpoint display
         ;; (commented out to allow nightly testing)
         #;"display-break-stuff.ss"
         (only-in "my-macros.ss" let*-2vals))

(define program-expander-contract
  ((-> void?) ; init
   ((or/c eof-object? syntax? (cons/c string? any/c)) (-> void?)
                                                      . -> . void?) ; iter
   . -> .
   void?))

(provide/contract
 [go (program-expander-contract       ; program-expander
      (step-result? . -> . void?)     ; receive-result
      (or/c render-settings? false/c) ; render-settings
      boolean?                        ; track-inferred-names?
      (or/c object? (symbols 'testing))   ;; FIXME: can do better: subclass of language%       ; the language level
      boolean?                        ; disable-error-handling (to allow debugging)
      . -> .
      void?)])

   
(define-struct posn-info (posn span))

(provide (struct-out posn-info))


; go starts a stepper instance
; see provide stmt for contract
(define (go program-expander receive-result render-settings
            show-lambdas-as-lambdas? language-level disable-error-handling)
  
  ;; finished-exps:
  ;;   (listof (list/c syntax-object? (or/c number? false?)( -> any)))
  ;; because of mutation, these cannot be fixed renderings, but must be
  ;; re-rendered at each step.
  (define finished-exps null)
  (define finished-defines (make-weak-hash))
  
  #;(define (unwind-finished stx)
    (printf "unwinding finished: ~a\n" (syntax->datum stx))
    (syntax-case stx (! #%top #%app)
      [(#%app ! e) (unwind-finished #'e)]
      [(#%top . g) #'g]
      [else
       (if (pair? (syntax-e stx))
           (datum->syntax
            stx (syntax-pair-map (syntax-e stx) (lambda (stx) (unwind-finished stx))) stx stx)
           stx)]))
  
  (define/contract add-to-finished
    ((-> syntax?) (or/c (listof natural-number/c) false/c) (-> any)
                  . -> . void?)
    (lambda (exp-thunk lifting-indices getter)
      #;(let ([exp (exp-thunk)])
        (syntax-case exp ()
          [((v) body)
           (stepper-syntax-property exp 'comes-from-lazy)
           (begin
             (printf "adding to hash ~a = ~a\n" 
                     (syntax->datum #'v) 
                     (syntax->datum #'body))
             (hash-set! finished-defines 
                        (syntax->datum #'v) 
                        #'body))]
          [else (printf "not adding to hash: ~a\n" (syntax->datum exp))]))
      (set! finished-exps
            (append finished-exps
                    (list (list exp-thunk lifting-indices getter))))))
  
  (define (deref-if-lazy-app stx)
    (define (transfer-highlight from-stx to-stx)
      (if (stepper-syntax-property from-stx 'stepper-highlight)
          (stepper-syntax-property to-stx 'stepper-highlight #t)
          to-stx))
    (if (stepper-syntax-property stx 'comes-from-lazy-app-let-body)
        (let* ([stxlst (syntax->list stx)]
               [op (car stxlst)]
               [args (cdr stxlst)]
               [derefed-args 
                (map 
                 (λ (x) 
                   (if (stepper-syntax-property x 'comes-from-lazy-app-let-body)
                       (deref-if-lazy-app x)
                       (let ([val (hash-ref finished-defines (syntax->datum x) (λ () #f))])
                         (if val (transfer-highlight x val) x))))
                 args)])
             (transfer-highlight stx (datum->syntax stx (cons op derefed-args))))
        stx))
  
  
  ;; the "held" variables are used to store the "before" step.
  (define held-exp-list the-no-sexp)
  
  (define-struct held (exps was-app? source-info))
  
  (define held-finished-list null)
  
  (define last-rhs the-no-sexp)
  (define last-rhs-finished null)
  
  
  ;; number of steps to skip (2010-07-30: current only used by lazy racket stepper)
  ;; uses held-exp-list and held-finished while skipping steps
  ;; steps-to-skip is a list of integers, where each element represents steps to skip
  ;; When the first element of the list is 0, send a "step" to be displayed
  ;; When the list is empty, stop skipping
  (define steps-to-skip '())
  
  (define (steps-to-skip?)
    (not (empty? steps-to-skip)))
  
  (define (decrement-steps-to-skip)
    (set! steps-to-skip (cons (sub1 (car steps-to-skip)) 
                              (cdr steps-to-skip))))
  ;; used to handle normal skipped-steps -- so they dont conflict with steps-to-skip
  (define (increment-steps-to-skip)
    (set! steps-to-skip (cons (add1 (car steps-to-skip))
                              (cdr steps-to-skip))))
  
  (define (end-of-steps-to-skip?)
    (zero? (car steps-to-skip)))
  
 
  
  ;; highlight-mutated-expressions :
  ;;   ((listof (list/c syntax? syntax?)) (listof (list/c syntax? syntax?))
  ;;   -> (list/c (listof syntax?) (listof syntax?)))
  ;; highlights changes occurring due to mutation.  This function accepts the
  ;; left-hand-side expressions and the right-hand-side expressions, and
  ;; matches them against each other to see which ones have changed due to
  ;; mutation, and highlights these.
  ;; POSSIBLE RESEARCH POINT: if, say, (list 3 4) is mutated to (list 4 5),
  ;;   should the 4 & 5 be highlighted individually or should the list as a
  ;;   whole be highlighted.  Is either one "wrong?"  equivalences between
  ;;   reduction semantics?
  ;;
  ;; 2005-11-14: punting. just highlight the whole darn thing if there are
  ;; any differences.  In fact, just test for eq?-ness.
  
  #;
  (define (highlight-mutated-expressions lefts rights)
    (if (or (null? lefts) (null? rights))
        (list lefts rights)
        (let ([left-car (car lefts)]
              [right-car (car rights)])
          (if (eq? (syntax-property left-car 'user-source)
                   (syntax-property right-car 'user-source))
              (let ([highlights-added
                     (highlight-mutated-expression left-car right-car)]
                    [rest (highlight-mutated-expressions
                           (cdr lefts) (cdr rights))])
                (cons (cons (car highlights-added) (car rest))
                      (cons (cadr highlights-added) (cadr rest))))))))
  
  ;; highlight-mutated-expression: syntax? syntax? -> syntax?
  ;; given two expressions, highlight 'em both if they differ at all.
  
  ;; notes: wanted to use simple "eq?" test... but this will fail when a
  ;; being-stepped definition (e.g.  in a let) turns into a permanent one.
  ;; We pay a terrible price for the lifting thing.  And, for the fact that
  ;; the highlighting follows from the reductions but can't obviously be
  ;; deduced from them.
  
  #;
  (define (highlight-mutated-expression left right)
    (cond
      ;; if either one is already highlighted, leave them alone.
      [(or (stepper-syntax-property left 'stepper-highlight)
           (stepper-syntax-property right 'stepper-highlight))
       (list left right)]
      
      ;; first pass: highlight if not eq?.  Should be broken for local-bound
      ;; things as they pass into permanence.
      [(eq? left right)
       (list left right)]
      
      [else (list (stepper-syntax-property left 'stepper-highlight)
                  (stepper-syntax-property right 'stepper-highlight))]))
  
  ;; mutated on receipt of a break, used in displaying breakpoint stuff.
  (define steps-received 0)
  
  (define break
    (lambda (mark-set break-kind [returned-value-list #f])
      (printf "\n---------- BREAK TYPE = ~a ----------\n" break-kind)
      (set! steps-received (+ steps-received 1))
      ;; have to be careful else this won't be looked up right away:
      ;; (commented out to allow nightly tests to proceed, 2007-09-04
      #;(when (getenv "PLTSTEPPERUNSAFE")
          (let ([steps-received/current steps-received])
            (run-on-drscheme-side
             (lambda ()
               (display-break-stuff
                steps-received/current
                mark-set break-kind returned-value-list)))))
      
      (let* ([mark-list (and mark-set (extract-mark-list mark-set))]
             [tmp0 (printf "MARKLIST:\n")]
             [tmp (and mark-set
                       (map (λ (x) (printf "~a\n" (display-mark x))) mark-list))]
             [tmp2 (printf "RETURNED VALUE LIST: ~a\n" returned-value-list)])
        
        (define (reconstruct-all-completed)
          (printf "\n    ##### reconstruct-all-completed #####\n")
          (begin0
          (filter-map
           (match-lambda
             [(list source-thunk lifting-indices getter)
              (let* ([source (source-thunk)])
                (if (r:hide-completed? source)
                    (begin
                      (printf "hidden, before reconstruct: ~a\n" (syntax->datum source))
                      (match 
                          (r:reconstruct-completed
                           source lifting-indices
                           getter render-settings)
                        [(vector exp b) 
                         (begin
                           (printf "hidden, after reconstruct, preunwound: ~a\n" (syntax->datum exp))
                           (printf "hidden, after reconstruct, unwound: ~a\n" (unwind exp render-settings)) )])
                      #f)
                    (match (r:reconstruct-completed
                            source lifting-indices
                            getter render-settings)
                      [(vector exp #f) (begin
                                         (printf "preunwound: ~a\n" (syntax->datum exp))
                                         (unwind exp render-settings)
                                         )]
                      [(vector exp #t) (begin
                                         (printf "notunwound: ~a\n" (syntax->datum exp))
                                         exp
                                         )])))])
           finished-exps)
          (printf "           #####################\n\n")))
        
        #;(>>> break-kind)
        #;(fprintf (current-error-port) "break called with break-kind: ~a ..." break-kind)
        (if (r:skip-step? break-kind mark-list render-settings)
            (begin
              (printf "___ SKIP ___\n")
              #;(fprintf (current-error-port) " but it was skipped!\n")
              (when (or (eq? break-kind 'normal-break)
                        ;; not sure about this...
                        (eq? break-kind 'nomal-break/values))
                (if (steps-to-skip?)
                    (increment-steps-to-skip)
                    (begin
                      (printf "held set to the-skipped-step\n")
                      (set! held-exp-list the-skipped-step)))))
            
            (begin
              #;(fprintf (current-error-port) "and it wasn't skipped.\n")
              (case break-kind
                [(normal-break normal-break/values)
                 (begin
                   (when (and (eq? break-kind 'normal-break)
                              returned-value-list)
                     (error 'break
                            "broken invariant: normal-break can't have returned values"))
                   (let*-2vals
                    ([left-side
                      (r:reconstruct-left-side mark-list returned-value-list render-settings)]
                     [print-left-side (printf "left side:\n  ~a\n" (syntax->datum left-side))]
                     [(context-records highlight) (find-highlight left-side)]
                     [lazy-app-op? (stepper-syntax-property highlight 'lazy-app-op)]
                     ;[tmp (printf "lazy-app-op? = ~a\n" lazy-app-op?)]
                     ;[tmp2 (printf "terms in app: ~a\n" 
;                                   (and lazy-app-op?
;                                        (length (syntax->list (mark-source (cadr mark-list))))))]
                     [left-side-derefed (deref-if-lazy-app left-side)]
                     [left-side-derefed-unwound
                      (map (λ (exp) (unwind exp render-settings))
;                           (maybe-lift left-side-derefed #f))]
                           (maybe-lift left-side #f))]
                     [print-lhs-unwound (for-each (λ (x) (printf "left side unwound: ~a\n" (syntax->datum x))) left-side-derefed-unwound)]
                     [comes-from-lazy?
;                      (stepper-syntax-property left-side 'comes-from-lazy-app-let-body)]
                      (stepper-syntax-property highlight 'comes-from-lazy)]
                     [new-finished-list (reconstruct-all-completed)]
                     [posn-info (mark-list->posn-info mark-list)]
                     [new-held (make-held
                                left-side-derefed-unwound
                                (r:step-was-app? mark-list)
                                posn-info)]
                     )
                    (and (steps-to-skip?) 
                         (end-of-steps-to-skip?)
;                         lazy-app-op?
                         comes-from-lazy?
                         (match held-exp-list
                           [(struct held (held-exps held-step-was-app? held-posn-info))
                            (receive-result
                             (make-before-after-result
                              (append held-finished-list held-exps)
                              (append new-finished-list left-side-derefed-unwound)
                              'normal
                              held-posn-info
                              posn-info))]))
                    
                    (if (and (steps-to-skip?)
;                             (not lazy-app-op?))
                             (not comes-from-lazy?))
                        (printf "skipping and keeping held the same\n")
                        (begin
                          (set! held-finished-list new-finished-list)
                          (set! held-exp-list new-held)))

                    ;; skip some steps
;                    (and lazy-app-op?
                    #;(and comes-from-lazy?
                         (set! steps-to-skip 
;                               (cons (sub1 (length (syntax->list (mark-source (cadr mark-list)))))
                               (cons 1
                                     steps-to-skip)))
                    
                    ))]
                
                [(result-exp-break result-value-break)
                 (if (and (steps-to-skip?)
                          (not (end-of-steps-to-skip?)))
                     (begin
                       (printf "steps-to-skip: ~a\n" steps-to-skip)
                       (decrement-steps-to-skip))
                     ; else steps-to-skip is 0 or #f
                 (let* ([reconstruct 
                        (lambda ()
                          (map (lambda (exp)
                                 (unwind exp render-settings))
                               (maybe-lift
                                (r:reconstruct-right-side
                                 mark-list returned-value-list render-settings)
                                #f)))]
                       [send-result (lambda (result)
                                      (set! held-exp-list the-no-sexp)
                                      (receive-result result))]
                       [tmp1 (printf "right side:\n  ~a\n" 
                                     (syntax->datum 
                                      (r:reconstruct-right-side mark-list returned-value-list render-settings)))])
                 (match held-exp-list
                   [(struct skipped-step ())
                     ;; don't render if before step was a skipped-step
                    (printf "held = skipped step\n")
                    (set! held-exp-list the-no-sexp)]
                   [(struct no-sexp ())
                    ;; in this case, there was no "before" step, due
                    ;; to unannotated code.  In this case, we make the
                    ;; optimistic guess that none of the finished
                    ;; expressions were mutated.  It would be somewhat
                    ;; painful to do a better job, and the stepper
                    ;; makes no guarantees in this case.
                    (printf "held = no sexp\n")
                    (let* ([use-lhs-ellipses?
                            (not
                             (stepper-syntax-property 
;                              (car (reconstruct))
                              (mark-source (car mark-list))
                              'dont-use-ellipses))]
                           [tmp (printf "use ellipses on lhs? = ~a\n" use-lhs-ellipses?)]
                           [new-rhs (reconstruct)]
                           [new-finished (reconstruct-all-completed)])
                      (if use-lhs-ellipses?
                          (begin
                            (send-result 
                             (make-before-after-result
                              ;; NB: this (... ...) IS UNRELATED TO 
                              ;; THE MACRO IDIOM OF THE SAME NAME
                              (list #`(... ...))
                              (append new-finished new-rhs)
                              'normal
                              #f #f))
                            (set! last-rhs new-rhs)
                            (set! last-rhs-finished new-finished))
                          (let*
                              ([print-msg (printf "lhs = no sexp, but not using ellipses\n")]
                               [lhs-datum (map (λ (x) (syntax->datum x)) last-rhs)]
                               [rhs-datum (map (λ (x) (syntax->datum x)) new-rhs)]
                               [print-left (printf "left side = ~a\n" lhs-datum)]
                               [print-right (printf "right side = ~a\n" rhs-datum)]
                               [left-equals-right? (equal? lhs-datum rhs-datum)]
                               [print-l=r (and left-equals-right?
                                               (printf "left and right side are identical, so skipping\n"))])
                            (and (not left-equals-right?)
                                 (send-result
                                  (make-before-after-result
                                   (append last-rhs-finished last-rhs)
                                   (append new-finished new-rhs)
                                   'normal
                                   #f #f))
                                 (set! last-rhs new-rhs)
                                 (set! last-rhs-finished new-finished)))))]
                   [(struct held (held-exps held-step-was-app? held-posn-info))
                    (printf "held = exps\n")
                    (let*-values
                        ([(step-kind)
                          (if (and held-step-was-app?
                                   (eq? break-kind 'result-exp-break))
                              'user-application
                              'normal)]
                         [(new-held) (reconstruct)]
                         [(new-finished) (reconstruct-all-completed)]
                         [(left-exps right-exps)
                          ;; write this later:
                          ;; (identify-changed
                          ;;  (append held-finished-list held-exps)
                          ;;  (append new-finished-list reconstructed))
                          (values (append held-finished-list
                                          held-exps)
                                  (append new-finished
                                          new-held))]
                         [(posn-info) (mark-list->posn-info mark-list)]
                         [(held-exps-datum) (map (λ (x) (syntax->datum x)) held-exps)]
                         [(new-held-datum) (map (λ (x) (syntax->datum x)) new-held)]
                         [(print-left) (printf "left side = ~a\n" held-exps-datum)]
                         [(print-right) (printf "right side = ~a\n" new-held-datum)]
                         [(left-equals-right?) (equal? held-exps-datum new-held-datum)]
                         [(print-left-right-eq) (printf "left-equals-right? = ~a\n" left-equals-right?)])
                      (and (not left-equals-right?)  ; left and right side can be same, depending on
                                                     ; how unwound (happens in lazy racket)
                           (set! last-rhs new-held)
                           (set! last-rhs-finished new-finished)
                           (receive-result
                            (make-before-after-result
                             left-exps right-exps step-kind 
                             held-posn-info
                             posn-info)))
                      (if (and (steps-to-skip?)
                               (end-of-steps-to-skip?))
                          (begin
                            (set! steps-to-skip (cdr steps-to-skip)) ; finish this skipping
                            (if (steps-to-skip?) ; if there are more to skip
                                (begin
                                  (printf "setting as held: ~a\n" (syntax->datum (cadr right-exps)))
                                  (set! held-exp-list 
                                        (make-held
                                         new-held
                                         (r:step-was-app? mark-list)
                                         posn-info))
                                  (set! held-finished-list new-finished))
                                (set! held-exp-list the-no-sexp)))
                          (set! held-exp-list the-no-sexp)) ; if we werent skipping
                      )])))]
                [(double-break)
                 ;; a double-break occurs at the beginning of a let's
                 ;; evaluation.
                 (when (and (not (steps-to-skip?))
                            (not (eq? held-exp-list the-no-sexp)))
                   (error
                    'break-reconstruction
                    "held-exp-list not empty when a double-break occurred"))
                 (let*-2vals
                  ([new-finished-list (reconstruct-all-completed)]
                   [reconstruct-result
                    (r:reconstruct-double-break mark-list render-settings)]
                   [tmp99 (printf "left (before unwind):\n  ~a\n" (syntax->datum (car reconstruct-result)))]
                   [tmp98 (printf "right (before unwind):\n  ~a\n" (syntax->datum (cadr reconstruct-result)))]
                   [left-side (map (lambda (exp) (unwind exp render-settings))
                                   (maybe-lift (car reconstruct-result) #f))]
                   [right-side (map (lambda (exp) (unwind exp render-settings))
                                    (maybe-lift (cadr reconstruct-result) #t))]
                   [tmp (map (λ (x) (printf "left side:\n  ~a\n" (syntax->datum x))) left-side)]
                   [tmp2 (map (λ (x) (printf "right side:\n  ~a\n" (syntax->datum x))) right-side)]
                   [(context-records highlight)
                    (find-highlight (car reconstruct-result))]
                   [tmp32 (printf "num terms in app: ~a\n" (length (syntax->list (unwind highlight render-settings))))]
                   [comes-from-lazy? ;(stepper-syntax-property (car left-side) 'comes-from-lazy)]
                    (stepper-syntax-property highlight 'comes-from-lazy)]
                   [testing (and comes-from-lazy?
                                 (printf "testing: ~a\n" (syntax->datum 
                                                          (unwind
                                                           (stepper-syntax-property
                                                            (datum->syntax #'tmp right-side)
                                                            'stepper-hint
                                                            (stepper-syntax-property highlight 'test-fn))
                                                           render-settings)
                                                           )))]
)
                  (let ([posn-info (mark-list->posn-info mark-list)])
                    (if comes-from-lazy?
                        (let ([new-held
                               (make-held 
                                left-side
                                (r:step-was-app? mark-list) ; hardcode #t here?
                                posn-info)])
                          (if (steps-to-skip?)
                              (match held-exp-list
                                [(struct held (held-exps held-step-was-app? held-posn-info))
                                 (receive-result
                                  (make-before-after-result
                                   (append held-finished-list held-exps)
                                   (append new-finished-list left-side)
                                   'normal
                                   held-posn-info
                                   posn-info))])
                              (void))
                          (set! steps-to-skip 
                                (cons (length (syntax->list (unwind highlight render-settings)))
                                      steps-to-skip))
                          (set! held-finished-list new-finished-list)
                          (map (λ (x) (printf "setting as held: ~a\n" (syntax->datum x))) left-side)
                          (set! held-exp-list new-held))
                        (receive-result
                         (make-before-after-result
                          (append new-finished-list left-side)
                          (append new-finished-list right-side)
                          'normal
                          posn-info
                          posn-info)))))]
                
                [(expr-finished-break)
                 (unless (not mark-list)
                   (error 'break
                          "expected no mark-list with expr-finished-break"))
                 ;; in an expr-finished-break, the returned-vals hold (listof
                 ;; (list/c source lifting-index getter)) this will now include
                 ;; define-struct breaks, for which the source is the source
                 ;; and the getter causes an error.
                 (for-each 
                  (λ (x) (printf "add to finished:\n  source:~a\n  index:~a\n  getter:~a\n" 
                                 (syntax->datum ((car x)))
                                 (second x)
                                 ((third x)))) 
                  returned-value-list)
                 (for-each (lambda (source/index/getter)
                             (apply add-to-finished source/index/getter))
                           returned-value-list)]
                
                [else (error 'break "unknown label on break")]))))))
  
  (define maybe-lift
    (if (render-settings-lifting? render-settings)
        lift
        ;; ... oh dear; model.ss should disable the double-break & late-let break when lifting is off.
        (lambda (stx dont-care) (list stx))))
  
  (define (step-through-expression expanded expand-next-expression)
    (let* ([annotated (a:annotate expanded break show-lambdas-as-lambdas?
                                  language-level)])
      (parameterize ([test-engine:test-silence #t])
        (eval-syntax annotated))
      (expand-next-expression)))
  
  (define (err-display-handler message exn)
    (match held-exp-list
      [(struct no-sexp ())
        (receive-result (make-error-result message))]
      [(struct held (exps dc posn-info))
       (begin
         (receive-result
          (make-before-error-result (append held-finished-list exps)
                                    message
                                    posn-info))
         (set! held-exp-list the-no-sexp))]))
  
  (program-expander
   (lambda ()
     (unless disable-error-handling
       (error-display-handler err-display-handler)))
   (lambda (expanded continue-thunk) ; iter
     (r:reset-special-values)
     (if (eof-object? expanded)
         (begin
           (receive-result (make-finished-stepping)))
         (step-through-expression expanded continue-thunk)))))


; no-sexp is used to indicate no sexpression for display.
; e.g., on an error message, there's no sexp.
(define-struct no-sexp ())
(define the-no-sexp (make-no-sexp))

; skipped-step is used to indicate that the "before" step was skipped.
(define-struct skipped-step ())
(define the-skipped-step (make-skipped-step))

;; produce a posn-info structure or false based on the information in a mark-list
;; mark-list->posn-info : (listof mark) -> (or/c posn-info? false?)
(define (mark-list->posn-info mark-list)
  (let* ([first-mark-source (mark-source (car mark-list))]
         [posn (syntax-position first-mark-source)]
         [span (syntax-span first-mark-source)])
    (if posn
        (make-posn-info posn span)
        #f)))
