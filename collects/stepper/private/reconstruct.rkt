; general assertions about reconstruction:
; a varref can only occur at the top of a mark-list
; a varref at the top of the mark-list must either be a top-level-variable
;  or have a value in some mark somewhere (or both).

(module reconstruct scheme/base
  (require (prefix-in kernel: syntax/kerncase)
           mzlib/list
           mzlib/etc
	   mzlib/contract
           scheme/match
           "marks.ss"
           "model-settings.ss"
           "shared.ss"
           "my-macros.ss"
           "lifting.rkt" ; for find-highlight
           (for-syntax scheme/base)
           racket/private/promise ; STC add
           #;(file "/Users/clements/clements/scheme-scraps/eli-debug.ss"))

  (provide/contract 
   [reconstruct-completed (syntax? 
                           (or/c (listof natural-number/c) false/c)
                           (-> (listof any/c))
                           render-settings? 
                           . -> .
                           (vector/c syntax? boolean?))]
   [hide-completed? (syntax? . -> . boolean?)]
   
   ;; front ends for reconstruct-current
   [reconstruct-left-side (mark-list?
                           (or/c (listof any/c) false/c)
                           render-settings?
                           . -> .
                           syntax?)]
   [reconstruct-right-side (mark-list?
                            (or/c (listof any/c) false/c)
                            render-settings?
                            . -> .
                            syntax?)]
   [reconstruct-double-break (mark-list?
                              render-settings?
                              . -> . 
                              (list/c syntax? syntax?))]
   
   [final-mark-list? (-> mark-list? boolean?)]
   [skip-step? (-> break-kind? (or/c mark-list? false/c) render-settings? boolean?)]
   [step-was-app? (-> mark-list? boolean?)]
   
   [reset-special-values (-> any)])
  
  (define nothing-so-far (gensym "nothing-so-far-"))

  ; the let-glump is a structure that contains the reconstruct-time data about
  ; a let-binding; that is, the names on the left-hand-side, the expression on
  ; the right-hand side, and the values computed.
  
  (define-struct let-glump (name-set exp val-set))

  ; split-list : ('a -> boolean) (listof 'a) -> (2vals (listof 'a) (listof 'a))
  ; split-list splits a list into two lists at the first element s.t. (fn element) => true).
  ; that is, split-list yields the lists A and B such that (append A B) gives the original
  ; list, and (fn element) => false for all elements in A, and B is either empty or
  ; (fn (car B)) => true
  
 (define (split-list fn lst)
    (let loop ([remaining lst] [so-far null]) 
      (cond [(null? remaining)
             (2vals (reverse so-far) null)]
            [else
             (if (fn (car remaining))
                 (2vals (reverse so-far) remaining)
                 (loop (cdr remaining) (cons (car remaining) so-far)))])))
  
  ; test cases
  ; (test (2vals '(93 4 2) '(0 2 1)) split-list (lambda (x) (= x 0)) '(93 4 2 0 2 1))
  ; (test (2vals '(3 4 5) '()) split-list (lambda (x) (= x 0)) '(3 4 5))
        
  ; n-split-list : num ('a list) -> ('a list) ('a list)
  ; n-split-list splits a given list A into two lists B and C, such that B contains the
  ; first n elements of A, and C contains the rest.

  (define (n-split-list num lst)
    (when (> num (length lst))
      (error 'n-split-list "can't split list ~a after ~ath element; not long enough" lst num))
    (let loop ([count num] [remaining lst] [so-far null])
      (if (= count 0)
          (2vals (reverse so-far) remaining)
          (loop (- count 1) (cdr remaining) (cons (car remaining) so-far)))))
  
  ; test cases
  ; (test (2vals '(a b c) '(d e f)) n-split-list 3 '(a b c d e f))
  
  
  (define (no-highlight? stx)
    (with-handlers ((exn:fail? (λ (x) #t)))
      (if (find-highlight stx) #f 42)))
  (define (mark-as-highlight stx)
    (stepper-syntax-property stx 'stepper-highlight #t))
  
  ; STC add
  ; extract-proc-if-struct : any -> procedure? or any
  ; Purpose: extracts closure from struct procedure (ie - lazy-proc in lazy racket)
  (define (extract-proc-if-struct f)
    (if (procedure? f)
        (let ([extracted (procedure-extract-target f)])
          (if extracted extracted f))
        f))
  ; extract-proc-if-promise : any -> thunk or any
  (define (extract-proc-if-promise p)
    (if (promise? p) 
        (extract-proc-if-promise (pref p))
        p))
  ; unwraps struct or promise around procedure
  (define (unwrap-proc f)
    (extract-proc-if-promise (extract-proc-if-struct f)))
    
  (define (new-promise-running? p)
    (if (promise? p)
        (let ([v (pref p)])
          (or (running? v)
              (and (promise? v)
                   (new-promise-running? v))))
        (raise-type-error 'new-promise-running? "promise" p)))
  
  (require (for-syntax mzlib/string)) ; for expr->string
  (define-syntax (pr stx)
    (syntax-case stx ()
      [(_ x)
       (with-syntax ([xx (datum->syntax #'x (expr->string (syntax->datum #'x)))])
       #'(printf (string-append xx " = ~a\n") x))]))
  (define (print-render-settings rs)
    (define-syntax (print-field stx)
      (syntax-case stx ()
        [(_ field)
         (with-syntax ([accessor (datum->syntax #'field
                                                (string->symbol
                                                 (string-append
                                                  "render-settings-"
                                                  (symbol->string (syntax->datum #'field)))))])
         #'(printf "~a = ~a\n" (quote field) (accessor rs)))]))
    
    (printf "RENDER SETTINGS:\n")
    (print-field true-false-printed?)
    (print-field constructor-style-printing?)
    (print-field abbreviate-cons-as-list?)
    (print-field render-to-sexp)
    (print-field lifting?)
    (print-field show-and/or-clauses-consumed?)
    (print-field all-bindings-mutable?)
    )
  ;               
 ;               
 ; ;;  ;;;    ;;;   ;;;   ; ;;         ;   ;   ;;;   ;  ;   ;   ;;;  
 ;;   ;   ;  ;     ;   ;  ;;  ;        ;   ;  ;   ;  ;  ;   ;  ;   ; 
 ;    ;   ;  ;     ;   ;  ;   ;         ; ;       ;  ;  ;   ;  ;   ; 
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ; ;    ;;;;  ;  ;   ;  ;;;;; 
 ;    ;      ;     ;   ;  ;   ;         ; ;   ;   ;  ;  ;   ;  ;     
 ;    ;      ;     ;   ;  ;   ;         ;;    ;   ;  ;  ;  ;;  ;     
 ;     ;;;;   ;;;   ;;;   ;   ;          ;     ;;;;; ;   ;; ;   ;;;; 
                                                                     
  ;; NaturalNumber -> syntax    
  (define (render-unknown-promise x)
    #`(quote #,(string->symbol (string-append "<DelayedEvaluation#" (number->string x) ">"))))
  
  ; recon-value print-converts a value.  If the value is a closure, recon-value
  ; prints the name attached to the procedure, unless we're on the right-hand-side
  ; of a let, or unless there _is_ no name.
  (require racket/set)
  (define seen-promises null)
  (define recon-value
    (opt-lambda (val render-settings [assigned-name #f] [recur? #f] [current-so-far nothing-so-far])
      (when (not recur?) (set! seen-promises (set))) ; seen-promises deals with infinite lists
      (if (hash-ref finished-xml-box-table val (lambda () #f))
          (stepper-syntax-property #`(quote #,val) 'stepper-xml-value-hint 'from-xml-box)
          (let ([closure-record 
                 (closure-table-lookup (unwrap-proc val) (lambda () #f))])
            (cond
              [closure-record
               (let* ([mark (closure-record-mark closure-record)]
                      [base-name (closure-record-name closure-record)])
                 (if base-name
                     (let* ([lifted-index (closure-record-lifted-index closure-record)]
                            [name (if lifted-index
                                      (construct-lifted-name base-name lifted-index)
                                      base-name)])
                       (if (and assigned-name (free-identifier=? base-name assigned-name))
                           (recon-source-expr (mark-source mark) (list mark) null null render-settings)
                           #`#,name))
                     (recon-source-expr (mark-source mark) (list mark) null null render-settings) ))]
              [(empty? val) #'empty] ; handle empty list separately
              [(list? val)
               (with-syntax ([(reconed-vals ...) 
                              (map (lx (recon-value _ render-settings #f #t current-so-far)) val)])
                 #'(list reconed-vals ...))]
              [(pair? val) ; handle improper lists
               (with-syntax ([reconed-car (recon-value (car val) render-settings #f #t current-so-far)]
                             [reconed-cdr (recon-value (cdr val) render-settings #f #t current-so-far)])
                 #'(#%plain-app cons reconed-car reconed-cdr))]
              [(promise? val) ; must be from library code, or running promise
               (let ([partial-eval-promise
                      (hash-ref partially-evaluated-promises-table
                                val (λ () #f))]
                     [partial-eval-promise2
                      ; i dont know why there is an extra layer around the promise but there is,
                      ; so I need to unwrap one layer
                      (hash-ref partially-evaluated-promises-table
                                (pref val) (λ () #f))])
                 (if partial-eval-promise 
                     partial-eval-promise
                     (if partial-eval-promise2
                         partial-eval-promise2
                         (if (and (new-promise-running? val) (not (eq? current-so-far nothing-so-far)))
                             (begin
                               (hash-set! partially-evaluated-promises-table val current-so-far)
                               (printf "using current-so-far\n")
                               current-so-far)
                             (if (and (new-promise-running? val) (not (null? last-so-far)))
                                 (begin (printf "using last-so-far\n")
                                 last-so-far)
               ; promise should never be running here
               (if (and (promise-forced? val) 
                        (not (new-promise-running? val)) 
                        (not (set-member? seen-promises val)))
                   (begin
                     (set! seen-promises (set-add seen-promises val)) ; deals with infinite lists
                     (recon-value (force val) render-settings #f #t current-so-far))
                   (if assigned-name
                       assigned-name
                   (let ([unknown-promise (hash-ref unknown-promises-table val (λ () #f))])
                     (if unknown-promise
                         (begin
                           (printf "unknown promise found: ~a\n" unknown-promise)
                           (render-unknown-promise unknown-promise)
                           )
                         (begin0
                           (render-unknown-promise next-unknown-promise)
                           (hash-set! unknown-promises-table val next-unknown-promise)
                           (set! next-unknown-promise (add1 next-unknown-promise))
                           (printf "next unknown promise: ~a\n" next-unknown-promise))))))) ))))]

              [else
               (let* ([rendered ((render-settings-render-to-sexp render-settings) val)])
                 (if (symbol? rendered)
                      #`#,rendered
                      #`(quote #,rendered)))])))))

  (define (final-mark-list? mark-list)
    (and (not (null? mark-list)) (eq? (mark-label (car mark-list)) 'final)))


       ;      ;                                                                     ;;;
       ;                                                          ;                    ;
  ;;;  ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;          ;;; ;;;;  ;;;   ; ;;;     ;
 ;     ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;          ;     ;   ;   ;  ;;   ;    ;
 ;     ; ;    ;  ;    ;          ;;     ;;     ;;           ;     ;   ;   ;  ;    ;   ;
  ;;   ;;     ;  ;    ;  ;;;;;   ;;     ;;     ;;    ;;;;;   ;;   ;   ;;;;;  ;    ;  ;
    ;  ; ;    ;  ;    ;          ;;     ;;     ;;              ;  ;   ;      ;    ;  ;
    ;  ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;             ;  ;   ;      ;;   ;
 ;;;   ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;         ;;;    ;;  ;;;;  ; ;;;   ;
                 ;                                                           ;
                 ;                                                           ;

  (define (skip-step? break-kind mark-list render-settings)
    (case break-kind
      [(result-value-break)
       (and (pair? mark-list)
            (let ([expr (mark-source (car mark-list))])
              (stepper-syntax-property expr 'stepper-hide-reduction)))]
      [(result-exp-break)
       ;; skip if clauses that are the result of and/or reductions
       (or (let ([and/or-clauses-consumed 
                  (stepper-syntax-property (mark-source (car mark-list)) 'stepper-and/or-clauses-consumed)])
             (and and/or-clauses-consumed
                  (> and/or-clauses-consumed 0)))
           (skip-lazy-redex-step? mark-list render-settings))]
      [(normal-break normal-break/values)
       (skip-redex-step? mark-list render-settings)]
      [(double-break)
       (or 
        ;; don't stop for a double-break on a let that is the expansion of a 'begin'
        (let ([expr (mark-source (car mark-list))])
          (or (eq? (stepper-syntax-property expr 'stepper-hint) 'comes-from-begin)
              (stepper-syntax-property expr 'stepper-skip-double-break)))
        (not (render-settings-lifting? render-settings)))]
      [(expr-finished-break define-struct-break late-let-break) #f]))
  
  (define (constructor-app? fn args mark-list render-settings)
    ; don't halt for proper applications of constructors
    (and 
     (identifier? fn) ; handle cases like (#%top . f)
     (let ([fun-val (extract-proc-if-struct (lookup-binding mark-list fn))])
       (and (procedure? fun-val)
            (procedure-arity-includes? 
             fun-val
             (length args))
            (or (and (render-settings-constructor-style-printing? render-settings)
                     (or (eq? fun-val special-list-value)
                         (eq? fun-val special-cons-value))
                     #;(if (render-settings-abbreviate-cons-as-list? render-settings)
                         (eq? fun-val special-list-value)
                         (and (eq? fun-val special-cons-value)
                              (second-arg-is-list? mark-list))))
                ;(model-settings:special-function? 'vector fun-val)
                (and (eq? fun-val void)
                     (eq? args null))
                (struct-constructor-procedure? fun-val))))))
    
    
    
  (define (skip-lazy-redex-step? mark-list render-settings)

    (let ([expr (mark-source (car mark-list))])
      (kernel:kernel-syntax-case
       expr #f
       [(#%plain-app (lam args body) fn . rest)
        (eq? (syntax->datum #'lam) 'lambda)
        (let* ([tmp (constructor-app? (skipto/auto #'fn 'discard (λ (x) x))
                                     (syntax->list #'rest)
                                     mark-list
                                     render-settings)])
          (if tmp
              (begin
                (printf "skipping lazy redex step\n")
                tmp)
              tmp))]
       [(#%plain-app (#%plain-app proc-extract-target p) . args)
        (eq? (syntax->datum #'proc-extract-target)
             'procedure-extract-target)
        (let ([tmp (constructor-app? #'p
                                      (syntax->list #'args)
                                      mark-list
                                      render-settings)])
          (if tmp
              (begin
                (printf "skipping lazy redex step\n")
                tmp)
              tmp))]
       [else #f])))
          
      
  ;; skip-redex-step : mark-list? render-settings? -> boolean?
  (define (skip-redex-step? mark-list render-settings)

    (define (varref-skip-step? varref)
      (with-handlers ([exn:fail:contract:variable? (lambda (dc-exn) #f)])
        (let ([val (lookup-binding mark-list varref)])
          (equal? (syntax->interned-datum 
                   (recon-value val render-settings))
                  (syntax->interned-datum 
                   (case (stepper-syntax-property varref 'stepper-binding-type)
                     ([let-bound]
                      (binding-lifted-name mark-list varref))
                     ([non-lexical]
                      varref)
                     (else
                      (error 'varref-skip-step? "unexpected value for stepper-binding-type: ~e for variable: ~e\n"
                             (stepper-syntax-property varref 'stepper-binding-type)
                             varref))))))))

    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (stepper-syntax-property expr 'stepper-hide-reduction)
               (skip-lazy-redex-step? mark-list render-settings)
               (kernel:kernel-syntax-case 
                expr #f
                [id
                 (identifier? expr)
                 (case (stepper-syntax-property expr 'stepper-binding-type)
                   [(lambda-bound) #t]  ; don't halt for lambda-bound vars
                   [(let-bound)
                    (varref-skip-step? expr)]
                   [(non-lexical)
                    (varref-skip-step? expr)])]
                [(#%top . id-stx)
                 (varref-skip-step? #`id-stx)]
                [(#%plain-app . terms)
                 ; don't halt for proper applications of constructors
                 (let ([tmp
                 (constructor-app? (get-arg-var 0)
                                   (cdr (syntax->list #'terms))
                                   mark-list
                                   render-settings)])
                   (if tmp (begin (printf "constructor app, skipping\n") tmp) tmp))
                 #;(let ([fun-val (lookup-binding mark-list (get-arg-var 0))])
                   (and (procedure? fun-val)
                        (procedure-arity-includes? 
                         fun-val
                         (length (cdr (syntax->list (syntax terms)))))
                        (or (and (render-settings-constructor-style-printing? render-settings)
                                 (if (render-settings-abbreviate-cons-as-list? render-settings)
                                     (eq? fun-val special-list-value)
                                     (and (eq? fun-val special-cons-value)
                                          (second-arg-is-list? mark-list))))
                            ;(model-settings:special-function? 'vector fun-val)
                            (and (eq? fun-val void)
                                 (eq? (cdr (syntax->list (syntax terms))) null))
                            (struct-constructor-procedure? fun-val))))]
                [else #f])))))
  
  ;; find-special-value finds the value associated with the given name.  Applications of functions
  ;; like 'list' should not be shown as steps, because the before and after steps will be the same.
  ;; it might be easier simply to discover and discard these at display time.
  (define (find-special-value name valid-args)
    (let* ([expanded-application (expand (cons name valid-args))]
           [stepper-safe-expanded (skipto/auto expanded-application 'discard (lambda (x) x))]
           [just-the-fn (kernel:kernel-syntax-case 
                         stepper-safe-expanded #f
                         [(#%plain-app 
                           (#%plain-app toplevelforcer)
                           (#%plain-app (lam args body) fn . rest))
                          (and (eq? (syntax->datum #'toplevelforcer) 'toplevel-forcer)
                               (eq? (syntax->datum #'lam) 'lambda))
                          (skipto/auto #'fn 'discard (λ (x) x))] ; lazy racket
                         [(#%plain-app fn . rest) #'fn]
                         ;[(let-values . rest) #f] ; so lazy racket doesnt give error
                         [else (error 'find-special-name "couldn't find expanded name for ~a" name)])])
;      (if just-the-fn
          (extract-proc-if-struct 
           (eval (syntax-recertify just-the-fn expanded-application (current-code-inspector) #f))) ))
;          #f))) ; so lazy racket doesnt give error

  ;; these are delayed so that they use the userspace expander.  I'm sure
  ;; there's a more robust & elegant way to do this.
  (define special-list-value #f)
  (define special-cons-value #f)
  
  ; This is used when we need the exp associated with a running promise, but the promise is at top-level,
  ; so it never gets added to partially-evaluated-promises-table
  ; This is a huge hack and I dont know if it the assumptions I'm making always hold
  ;  (ie - that the exp associated with any running promise not in partially-evaluated-promises-table is the last so-far), 
  ;  but it's working for all test cases so far 10/29/2010.
  ;  Another solution is to wrap all lazy programs in a dummy top-level expression???
  ;  Update 11/1/2010: needed to add the following guards in the code to make the assumptions hold 
  ;                    (guards are mainly triggered when there are infinite lists)
  ;  - in recon-inner, dont add running promise to partially-evaluated-promises-table if so-far = nothing-so-far
  ;  - in recon, dont set last-so-far when so-far = nothing-so-far 
  ;  - in recon-value, dont use last-so-far if it hasnt been set (ie - if it's still null)
  (define last-so-far null)
  
  (define (reset-special-values)
    (set! special-list-value (find-special-value 'list '(3)))
    (set! special-cons-value (find-special-value 'cons '(3 empty)))
    (set! unknown-promises-table (make-weak-hash))
    (set! next-unknown-promise 0))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (lookup-binding mark-list (get-arg-var 2))])
      (list? arg-val)))
    
;   ; static-binding-indexer (z:parsed -> integer)
;  
;  (define static-binding-indexer
;    (let* ([name-number-table (make-hash-table)]
;           [binding-number-table (make-hash-table-weak)])
;      (lambda (binding)
;        (cond [(hash-table-get binding-number-table binding (lambda () #f)) =>
;               (lambda (x) x)]
;              [else (let* ([orig-name (z:binding-orig-name binding)]
;                           [old-index (hash-table-get name-number-table orig-name (lambda () -1))]
;                           [new-index (+ old-index 1)])
;                      (hash-table-put! name-number-table orig-name new-index)
;                      (hash-table-put! binding-number-table binding new-index)
;                      new-index)]))))
  
  ; construct-lifted-name 
  ; (-> syntax? (or/c num? false/c) symbol?)
  
  (define/contract construct-lifted-name
    (-> syntax? number? syntax?)
    (lambda (binding dynamic-index)
      #`#,(string->symbol
           (string-append (symbol->string (syntax-e binding)) "_" 
                          (number->string dynamic-index)))))
  
  ; binding-lifted-name
  
  (define/contract binding-lifted-name
    (-> mark-list? syntax? syntax?)
    (lambda (mark-list binding)
      (construct-lifted-name binding (lookup-binding mark-list (get-lifted-var binding)))))

  
  (define (step-was-app? mark-list)
    (and (not (null? mark-list))
         (syntax-case (mark-source (car mark-list)) (#%plain-app)
           [(#%plain-app . rest)
            #t]
           [else
            #f])))

  ; weak hash table where keys = promises, values = syntax
  ; initialized in reconstruct-current
  (define partially-evaluated-promises-table null)
  
  ; unknown promises, generated from library (ie - non-user) code
  ;; reset in reset-special-values fn
  (define unknown-promises-table null)
  (define next-unknown-promise 0)
  
  #;(define not-yet-called-apps (make-hash))
                                                                                                               
 ; ;;  ;;;    ;;;   ;;;   ; ;;           ;;;   ;;;   ;   ;  ; ;;  ;;;   ;;;           ;;;  ;    ;  ; ;;;   ; ;;
 ;;   ;   ;  ;     ;   ;  ;;  ;         ;     ;   ;  ;   ;  ;;   ;     ;   ;         ;   ;  ;  ;   ;;   ;  ;;  
 ;    ;   ;  ;     ;   ;  ;   ;         ;     ;   ;  ;   ;  ;    ;     ;   ;         ;   ;   ;;    ;    ;  ;   
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;   ;;   ;   ;  ;   ;  ;    ;     ;;;;;  ;;;;;  ;;;;;   ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;   ;  ;    ;     ;             ;       ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;  ;;  ;    ;     ;             ;      ;  ;   ;;   ;  ;   
 ;     ;;;;   ;;;   ;;;   ;   ;         ;;;    ;;;    ;; ;  ;     ;;;   ;;;;          ;;;; ;    ;  ; ;;;   ;   
                                                                                                   ;           
                                                                                                   ;           
                                                                                                               

  ; recon-source-expr 
  
  ; recon-source-expr produces the reconstructed version of a given source epxression, using the binding
  ; information contained in the binding-list.  This happens during reconstruction whenever we come upon
  ; expressions that we haven't yet evaluated. 
  
  ; NB: the variable 'dont-lookup' contains a list of variables whose bindings occur INSIDE the expression
  ; being evaluated, and hence do NOT yet have values.
  
  ; the 'use-lifted-names' vars are those bound by a let which does have lifted names.  it is used in
  ; rendering the lifting of a let or local to show the 'after' step, which should show the lifted names.

  (define/contract recon-source-expr 
    (-> syntax? mark-list? binding-set? binding-set? render-settings? syntax?)
    (lambda (expr mark-list dont-lookup use-lifted-names render-settings)
      (skipto/auto
       expr
       'discard
       (lambda (expr)
         (if (stepper-syntax-property expr 'stepper-prim-name)
             (stepper-syntax-property expr 'stepper-prim-name)
             (let* ([recur 
                     (lambda (expr) 
                       (recon-source-expr expr mark-list dont-lookup use-lifted-names render-settings))]
                     [let-recur 
                      (lambda (expr bindings)
                        (recon-source-expr expr mark-list (append bindings dont-lookup) use-lifted-names render-settings))]
                     [recon-basic
                      (lambda ()
                        (with-syntax ([(label . bodies) expr])
                          #`(label #,@(map recur (filter-skipped (syntax->list (syntax bodies)))))))]
                     [recon-let/rec
                      (lambda (rec?)
                        (with-syntax ([(label  ((vars val) ...) body ...) expr])
                          (let* ([bindings (map syntax->list (syntax->list (syntax (vars ...))))]
                                 [binding-list (apply append bindings)]
                                 [recur-fn (if rec? 
                                               (lambda (expr) (let-recur expr binding-list))
                                               recur)]
                                 [right-sides (map recur-fn (syntax->list (syntax (val ...))))]
                                 [recon-bodies (map (lambda (x) (let-recur x binding-list))
                                                    (syntax->list #`(body ...)))])
                            (with-syntax ([(recon-val ...) right-sides]
                                          [(recon-body ...) recon-bodies]
                                          [(new-vars ...) 
                                           (map (lx (map (lx (if (ormap (lambda (binding)
                                                                          (bound-identifier=? binding _))
                                                                        use-lifted-names)
                                                                 (stepper-syntax-property _
                                                                                          'stepper-lifted-name
                                                                                          (binding-lifted-name mark-list _))
                                                                 _))
                                                         _))
                                                bindings)])
                              (syntax (label ((new-vars recon-val) ...) recon-body ...))))))]
                     [recon-lambda-clause
                      (lambda (clause)
                        (with-syntax ([(args . bodies-stx) clause])
                          (let* ([arglist (arglist-flatten #'args)]
                                 [bodies (map (lambda (body) (let-recur body arglist))
                                              (filter-skipped (syntax->list (syntax bodies-stx))))])
                            (cons (syntax args) bodies))))]
                     [recon (kernel:kernel-syntax-case expr #f
                              
                              ; lambda
                              [(#%plain-lambda . clause-stx)
                               (let* ([clause (recon-lambda-clause (syntax clause-stx))])
                                 #`(#%plain-lambda #,@clause))]
                              
                              ; case-lambda
                              [(case-lambda . clauses-stx)
                               (let* ([clauses (map recon-lambda-clause (syntax->list (syntax clauses-stx)))])
                                 #`(case-lambda #,@clauses))]
                              
                              ; if, begin, begin0
                              [(if test then else) (recon-basic)]
                              [(if test then) (recon-basic)]
                              [(begin . bodies) (recon-basic)]
                              [(begin0 . bodies) 
                               (if (stepper-syntax-property expr 'stepper-fake-exp)
                                   (if (null? (syntax->list #`bodies))
                                       (recon-value (lookup-binding mark-list begin0-temp) render-settings)
                                       ;; prepend the computed value of the first arg:
                                       #`(begin0 #,(recon-value (lookup-binding mark-list begin0-temp) render-settings)
                                                 #,@(map recur (filter-skipped (syntax->list #`bodies)))))
                                   (recon-basic))]
                              
                              ; let-values, letrec-values
                              [(let-values . rest) (recon-let/rec #f)]
                              [(letrec-values . rest) (recon-let/rec #t)]
                              
                              ; set! 
                              [(set! var rhs)
                               (let ([rendered-var 
                                      (if (and (ormap (lambda (binding)
                                                        (bound-identifier=? binding #`var))
                                                      dont-lookup)
                                               (not (ormap (lambda (binding)
                                                             (bound-identifier=? binding #`var))
                                                           use-lifted-names)))
                                          #`var
                                          (reconstruct-set!-var mark-list #`var))])
                                 #`(set! #,rendered-var #,(recur #'rhs)))]
                              
                              ; quote 
                              [(quote body) (recon-value (eval-quoted expr) render-settings)]
                              
                              ; quote-syntax : like set!, the current stepper cannot handle quote-syntax
                              
                              ; with-continuation-mark
                              [(with-continuation-mark . rest) (recon-basic)]
                              
                              ; application
                              [(#%plain-app . terms) (recon-basic)]
                              
                              ; varref                        
                              [var-stx
                               (identifier? expr)
                               (let* ([var (syntax var-stx)])
                                 (if (render-settings-all-bindings-mutable? render-settings)
                                     var
                                     (cond [(eq? (identifier-binding var) 'lexical)
                                            ; has this varref's binding not been evaluated yet?
                                            ; (and this varref isn't in the list of must-lookups?)
                                            (if (and (ormap (lambda (binding)
                                                              (bound-identifier=? binding var))
                                                            dont-lookup)
                                                     (not (ormap (lambda (binding)
                                                                   (bound-identifier=? binding var))
                                                                 use-lifted-names)))
                                                var
                                                
                                                (case (stepper-syntax-property var 'stepper-binding-type)
                                                  ((lambda-bound)
                                                   ; STC add
                                                   (let* ([val (lookup-binding mark-list var)]
                                                          [partial-eval-promise
                                                           (hash-ref partially-evaluated-promises-table
                                                                     val (λ () #f))])
                                                     (if #f ;partial-eval-promise
                                                         partial-eval-promise
                                                         (recon-value val render-settings))))
                                                  ((macro-bound)
                                                   ; for the moment, let-bound vars occur only in and/or :
                                                   (recon-value (lookup-binding mark-list var) render-settings))
                                                  ((let-bound)
                                                   (if (stepper-syntax-property var 'stepper-no-lifting-info)
                                                       var
                                                       (stepper-syntax-property var
                                                                                'stepper-lifted-name
                                                                                (binding-lifted-name mark-list var))))
                                                  ((stepper-temp)
                                                   (error 'recon-source-expr 
                                                          "stepper-temp showed up in source?!?"))
                                                  ((non-lexical)
                                                   (error 'recon-source-expr 
                                                          "can't get here: lexical identifier labeled as non-lexical"))
                                                  (else
                                                   (error 'recon-source-expr 
                                                          "unknown 'stepper-binding-type property: ~a on var: ~a" 
                                                          (stepper-syntax-property var 'stepper-binding-type) (syntax->datum var)))))]
                                           [else ; top-level-varref
                                            (fixup-name
                                             var)])))]
                              [(#%top . var)
                               (syntax var)]
                              
                              [else
                               (error 'recon-source "no matching clause for syntax: ~a" (if (syntax? expr)
                                                                                            (syntax->datum expr)
                                                                                            expr))])])
                (attach-info recon expr)))))))
  
  ;; reconstruct-set!-var
  
  (define (reconstruct-set!-var mark-list var)
    (case (stepper-syntax-property var 'stepper-binding-type)
        ((lambda-bound)
         (error 'reconstruct-inner "lambda-bound variables can't be mutated"))
        ((macro-bound)
         ; for the moment, let-bound vars occur only in and/or :
         (error 'reconstruct-inner "macro-bound variables can't occur in a set!"))
        ((non-lexical) var)
        ((let-bound)
         (stepper-syntax-property var
                          'stepper-lifted-name
                          (binding-lifted-name mark-list var)))
        ((stepper-temp)
         (error 'recon-source-expr "stepper-temp showed up in source?!?"))
        (else
         (error 'recon-source-expr "unknown 'stepper-binding-type property: ~a" 
                (stepper-syntax-property var 'stepper-binding-type)))))
  
  ;; filter-skipped : (listof syntax?) -> (listof syntax?)
  ;; filter out any elements of the list with 'stepper-skip-completely set, except those with stepper-prim-name set. (HACK).
  (define (filter-skipped los)
    (filter (lambda (stx)
              (or (stepper-syntax-property stx 'stepper-prim-name)
                  (not (stepper-syntax-property stx 'stepper-skip-completely))))
            los))
 
  
  ;; mflatt: MAJOR HACK - work around the prefix on
  ;;         beginner name definitions
  (define (fixup-name s)
    (let ([m (regexp-match re:beginner: (symbol->string (syntax-e s)))])
      (if m
	  (datum->syntax s (string->symbol (cadr m)) s s)
	  s)))
  (define re:beginner: (regexp "^beginner:(.*)$"))
  
 
  ;; eval-quoted : take a syntax that is an application of quote, and evaluate it (for display)
  ;; Frankly, I'm worried by the fact that this isn't done at expansion time.
  
  (define (eval-quoted stx)
    (syntax-case stx (quote)
      [(quote . dont-care) (eval stx)]
      [else (error 'eval-quoted "eval-quoted called with syntax that is not a quote: ~v" stx)]))
  
  
                                       ;                     ; 
                                       ;                     ;                                          ;         ;               ; 
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;   ;;;   ; ;;; ;;   ; ;;;   ;   ;;;  ;;;;  ;;;    ;;; ; 
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;  ;;  ;  ;;   ;  ;  ;   ;  ;   ;   ;  ;   ;; 
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;   ;  ;   ;   ;  ;    ; 
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;   ;   ;  ;    ;  ;  ;;;;;  ;   ;;;;;  ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;      ;   ;      ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;   ;  ;   ;   ;  ;;   ;  ;  ;      ;   ;      ;   ;; 
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;;;   ;   ;   ;  ; ;;;   ;   ;;;;   ;;  ;;;;   ;;; ; 
                                                                                                ;                                   
                                                                                                ;                                   
                                                                                                                                    

  ; reconstruct-completed : reconstructs a completed expression or definition.  
  ; Accepts the source expression, a lifting-index which is either a number (indicating
  ;  a lifted binding) or false (indicating a top-level expression), a list of values
  ;  currently bound to the bindings, and the language level's render-settings.
  ;; returns a vector containing a reconstructed expression and a boolean indicating 
  ;; whether this should not be unwound (e.g., is source syntax
  ;; from a define-struct).
  
  (define (reconstruct-completed exp lifting-indices vals-getter render-settings)
    (if lifting-indices
        (syntax-case exp ()
          [(vars-stx rhs ...)
           (let* ([vars (map (lambda (var index) (stepper-syntax-property var 'stepper-lifted-name (construct-lifted-name var index))) 
                             (syntax->list #`vars-stx)
                             lifting-indices)])
             (vector (reconstruct-completed-define exp vars (vals-getter) render-settings) #f))])
        (let ([exp (skipto/auto exp 'discard (lambda (exp) exp))])
          (cond 
            [(stepper-syntax-property exp 'stepper-define-struct-hint)
             ;; the hint contains the original syntax
             (vector (stepper-syntax-property exp 'stepper-define-struct-hint) #t)]
            ;; for test cases, use the result here as the final result of the expression:
            [(stepper-syntax-property exp 'stepper-use-val-as-final)
             (vector (recon-value (car (vals-getter)) render-settings) #f)]
            [else
             (vector
              (kernel:kernel-syntax-case exp #f
                [(define-values vars-stx body)
                 (reconstruct-completed-define exp (syntax->list #`vars-stx) (vals-getter) render-settings)]
                [else
                 (let* ([recon-vals (map (lambda (val)
                                           (recon-value val render-settings))
                                         (vals-getter))])
                   (if (= (length recon-vals) 1)
                       (attach-info (car recon-vals) exp)
                       (attach-info #`(values #,@recon-vals) exp)))])
              #f)]))))
  
  ;; an abstraction lifted from reconstruct-completed
  (define (reconstruct-completed-define exp vars vals render-settings)
    (let* ([_ (unless (equal? (length vars) (length vals))
                (error "length of var list and val list unequal: ~v ~v" (map syntax->list vars) vals))]
           [recon-vals (map (lambda (val var) 
                              (recon-value val render-settings var)) 
                            vals
                            vars)])
      (if (= (length recon-vals) 1)
          (attach-info #`(define-values #,vars #,(car recon-vals)) exp)
          (attach-info #'(define-values #,vars (values #,@recon-vals)) exp))))
  
  
  
  ; : (-> syntax? syntax? syntax?)
  (define (reconstruct-top-level source reconstructed)
    (skipto/auto
     source
     'discard
     (lambda (source)
       (kernel:kernel-syntax-case source #f
         [(define-values vars-stx body)
          (attach-info #`(define-values vars-stx #,reconstructed)
                       source)]
         [else
          reconstructed]))))
  
                                                   
  ;; hide-completed? : syntax? -> boolean?
  (define (hide-completed? stx)
    (syntax-case stx ()
      [(define-values (v) rhs)
       (stepper-syntax-property #'v 'stepper-hide-completed)]
      [else #f]))
      #;[else
       (stepper-syntax-property stx 'comes-from-lazy)]
  
                                                                                                                
                                       ;                     ;                                               ;  
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;  ;   ;  ; ;; ; ;;  ;;;   ; ;;  ;;;;
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;   ;;   ;   ;  ;;  ;  ;  
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;   ;  ;   ;  ;  
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;    ;    ;;;;;  ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;      ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;  ;;  ;    ;    ;      ;   ;  ;  
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;; ;  ;    ;     ;;;;  ;   ;   ;;
                      
  
  ;; front ends for reconstruct-current:
  
  (define (reconstruct-left-side mark-list returned-value-list render-settings)
    (reconstruct-current mark-list 'left-side returned-value-list render-settings))
  
  
  (define (reconstruct-right-side mark-list returned-value-list render-settings)
    (reconstruct-current mark-list 'right-side returned-value-list render-settings))
  
  
  (define (reconstruct-double-break mark-list render-settings)
    (reconstruct-current mark-list 'double-break null render-settings))
                                                                                                                           
  
  ; reconstruct-current : takes a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list of sexps
  ; (the result of reconstruction) --- which may contain holes, indicated by the 
  ; highlight-placeholder --- and a list of sexps which go in the holes
  
  (define (reconstruct-current mark-list break-kind returned-value-list render-settings)
    (local
        (
         
         ; ;;  ;;;    ;;;   ;;;   ; ;;          ;  ; ;;   ; ;;    ;;;   ; ;;
         ;;   ;   ;  ;     ;   ;  ;;  ;         ;  ;;  ;  ;;  ;  ;   ;  ;;  
         ;    ;   ;  ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;   ;  ;   
         ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ;  ;   ;  ;   ;  ;;;;;  ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;     ;;;;   ;;;   ;;;   ;   ;         ;  ;   ;  ;   ;   ;;;;  ;   
         
         
         (define (recon-inner mark-list so-far)
           (if (not (eq? so-far nothing-so-far))
               (printf "recon-inner, so-far: ~a\n" (syntax->hilite-datum so-far))
               (printf "recon-inner, so-far: nothing-so-far\n"))
           (let* ([recon-source-current-marks 
                   (lambda (expr)
                     (recon-source-expr expr mark-list null null render-settings))]
                  [top-mark (car mark-list)]
                  [exp (mark-source top-mark)]
                  [dont-use-ellipses?
                   (stepper-syntax-property exp 'dont-use-ellipses)]
                  [maybe-running-promise
                   (findf (λ (f) (and (promise? f) (new-promise-running? f)))
                          (map mark-binding-value (mark-bindings top-mark)))]
                  [tmp-caching-running-promise
                   (when (and maybe-running-promise
                              (not (hash-has-key? partially-evaluated-promises-table
                                                  maybe-running-promise))
                              (not (eq? so-far nothing-so-far)))
                     (hash-set! partially-evaluated-promises-table
                                maybe-running-promise so-far))]
                  [iota (lambda (x) (build-list x (lambda (x) x)))]
                  
                  [recon-let
                   (lambda ()
                     (with-syntax ([(label ((vars rhs) ...) . bodies) exp])
                       (let*-2vals 
                        ([binding-sets (map syntax->list (syntax->list #'(vars ...)))]
                         [binding-list (apply append binding-sets)]
                         [glumps 
                          (map (lambda (binding-set rhs)
                                 (make-let-glump
                                  (map (lambda (binding)
                                         (stepper-syntax-property 
                                          binding
                                          'stepper-lifted-name
                                          (binding-lifted-name mark-list binding)))
                                       binding-set)
                                  rhs
                                  (map (lambda (arg-binding) 
                                         (lookup-binding mark-list arg-binding))
                                       binding-set)))
                               binding-sets
                               (syntax->list #`(rhs ...)))]
                         [num-defns-done (lookup-binding mark-list let-counter)]
                         [(done-glumps not-done-glumps)
                          (n-split-list num-defns-done glumps)]
                         [recon-lifted 
                          (lambda (names expr)
                            #`(#,names #,expr))]
                         [before-bindings
                          (map
                           (lambda (glump)
                             (let* ([name-set (let-glump-name-set glump)]
                                    [rhs-val-set 
                                     (map 
                                      (lambda (val)
                                        (if (> (length name-set) 0)
                                            (recon-value val render-settings (car name-set))
                                            (recon-value val render-settings))) 
                                      (let-glump-val-set glump))])
                               (if (= (length rhs-val-set) 1)
                                   #`(#,name-set #,@rhs-val-set)
                                   #`(#,name-set (values #,rhs-val-set)))))
                           done-glumps)]
                         [reconstruct-remaining-def
                          (lambda (glump)
                            (let ([rhs-source (let-glump-exp glump)]
                                  [rhs-name-set (let-glump-name-set glump)])
                              (recon-lifted rhs-name-set
                                            (recon-source-current-marks rhs-source))))]
                         [after-bindings
                          (if (pair? not-done-glumps)
                              (if (eq? so-far nothing-so-far)
                                  (map reconstruct-remaining-def not-done-glumps)
                                  (cons (recon-lifted (let-glump-name-set (car not-done-glumps)) so-far)
                                        (map reconstruct-remaining-def (cdr not-done-glumps))))
                              null)]
                         [recon-bindings (append before-bindings after-bindings)]
                         ;; there's a terrible tangle of invariants here.  Among them:  
                         ;; num-defns-done = (length binding-sets) IFF the so-far has a 'stepper-offset' index
                         ;; that is not #f (that is, we're evaluating the body...)                                    
                         [so-far-offset-index 
                          (and (not (eq? so-far nothing-so-far)) 
                               (stepper-syntax-property so-far 'stepper-offset-index))]
                         [bodies (syntax->list (syntax bodies))]
                         [rectified-bodies 
                          (map (lambda (body offset-index)
                                 (if (eq? offset-index so-far-offset-index)
                                     so-far
                                     (recon-source-expr body mark-list binding-list binding-list render-settings)))
                               bodies
                               (iota (length bodies)))])
                        (attach-info #`(label #,recon-bindings #,@rectified-bodies) exp))))])
             (if (stepper-syntax-property exp 'stepper-fake-exp)
                 
                 (kernel:kernel-syntax-case exp #f
                   [(begin . bodies)
                    (if (eq? so-far nothing-so-far)
                        (error 'recon-inner 
                               "breakpoint before a begin reduction should have a result value in exp: ~a" (syntax->datum exp))
                        #`(begin #,so-far #,@(map recon-source-current-marks (cdr (syntax->list #'bodies)))))]
                   [(begin0 first-body . rest-bodies)
                    (if (eq? so-far nothing-so-far)
                        (error 'recon-inner 
                               "breakpoint before a begin0 reduction should have a result value in exp: ~a" (syntax->datum exp))
                        #`(begin0 #,(recon-value (lookup-binding mark-list begin0-temp) render-settings)
                                  #,so-far
                                  #,@(map recon-source-current-marks (syntax->list #`rest-bodies))))]
                   [else
                    (error 'recon-inner "unexpected fake-exp expression: ~a" (syntax->datum exp))])
                 
                 (kernel:kernel-syntax-case 
                  exp #f 
                  ; variable references
                  [id
                   (identifier? (syntax id))
                   (if (eq? so-far nothing-so-far)
                       (recon-source-current-marks exp)
                       (error 'recon-inner "variable reference given as context: ~a" exp))]
                                            
                  [(#%top . id)
                   (if (eq? so-far nothing-so-far)
                       (recon-source-current-marks exp)
                       (error 'recon-inner "variable reference given as context: ~a" exp))]
                  
                  ; applications
                  [(#%plain-app . terms)
                   (attach-info
                    (match-let* 
                        ([sub-exprs (syntax->list (syntax terms))]
                         [arg-temps (build-list (length sub-exprs) get-arg-var)]
                         [arg-vals (map (lambda (arg-temp) 
                                          (lookup-binding mark-list arg-temp))
                                        arg-temps)]
                         [(vector evaluated unevaluated) 
                          (split-list (lambda (x) (eq? (cadr x) *unevaluated*))
                                      (zip sub-exprs arg-vals))]
                         [rectified-evaluated 
                          (map (lx (recon-value _ render-settings)) 
                               (map cadr evaluated))])
                      (case (mark-label (car mark-list))
                        ((not-yet-called)
                         (if (null? unevaluated)
                             #`(#%plain-app . #,rectified-evaluated)
                             ; STC added let
                             (let*
                                 ([current-subterm
                                   (skipto/auto (caar unevaluated) 'discard (λ (x) x))]
                                  [current-binding-maybe
                                   (if (identifier? current-subterm)
                                       current-subterm
                                       #f)]
                                  [current-val
                                   (and
                                    current-binding-maybe
                                    (lookup-binding mark-list current-binding-maybe))])
                               #`(#%plain-app 
                                  #,@rectified-evaluated
                                  #,so-far 
                                  #,@(map recon-source-current-marks (cdr (map car unevaluated))))
                               ) ))
                        ((called)
                         (stepper-syntax-property
                          (if (eq? so-far nothing-so-far)
                              (datum->syntax #'here `(,#'#%plain-app ...)) ; in unannotated code ... can this occur?
                              (if dont-use-ellipses?
                                  (let* ([vals
                                          (build-list
                                           (sub1 ; dont want to include #%app
                                            (length (syntax->list 
                                                     (mark-source 
                                                      (car mark-list)))))
                                           (λ (n)
                                             (lookup-binding mark-list 
                                                             (get-arg-var n))))]
                                         ; I'm assuming hole must be where running promise is
                                         [reconed-vals-with-so-far-inserted
                                          (map 
                                           (λ (v) (recon-value v render-settings #f #f so-far))
                                             #;(if (and (promise? v) 
                                                      (promise-running? v))
                                                 so-far
                                                 (recon-value v render-settings))
                                               vals)])
                                    #`(#%plain-app #,@reconed-vals-with-so-far-inserted))
                                  (datum->syntax #'here `(,#'#%plain-app ... ,so-far ...))))
                          'stepper-args-of-call 
                          rectified-evaluated))
                        (else
                         (error 'recon-inner "bad label (~v) in application mark in expr: ~a" (mark-label (car mark-list)) exp))))
                    exp)]
                                            
                                            ; define-struct 
                                            ;               
                                            ;               [(z:struct-form? expr)
                                            ;                 (if (comes-from-define-struct? expr)
                                            ;                     so-far
                                            ;                     (let ([super-expr (z:struct-form-super expr)]
                                            ;                           [raw-type (utils:read->raw (z:struct-form-type expr))]
                                            ;                           [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                                            ;                       (if super-expr
                                            ;                           `(struct (,raw-type ,so-far)
                                            ;                                    ,raw-fields)
                                            ;                           `(struct ,raw-type ,raw-fields))))]
                                            
                                            ; if
                                            [(if test then else)
                                             (begin
                                               (when (eq? so-far nothing-so-far)
                                                 (error 'reconstruct "breakpoint before an if reduction should have a result value"))
                                               (attach-info
                                                #`(if #,so-far
                                                      #,(recon-source-current-marks (syntax then))
                                                      #,(recon-source-current-marks (syntax else)))
                                                exp))]
                                            
                                            ; one-armed if
                                            
                                            [(if test then)
                                             (begin
                                               (when (eq? so-far nothing-so-far)
                                                 (error 'reconstruct "breakpoint before an if reduction should have a result value"))
                                               (attach-info
                                                #`(if #,so-far #,(recon-source-current-marks (syntax then)))
                                                exp))]
                                            
                                            ; quote : there is no break on a quote.
                                            
                                            ;; advanced-begin : okay, here comes advanced-begin.
                                            
                                            [(begin . terms)
                                             ;; even in advanced, begin expands into a let-values.
                                             (error 'reconstruct/inner "begin in non-teaching-languages not implemented in reconstruct")]
                                            
                                            ; begin : in the current expansion of begin, there are only two-element begin's, one-element begins, and 
                                            ;; zero-element begins; these arise as the expansion of ... ?
                                            
                                            ;; these are all dead code, right?
                                            
                                            #;[(begin stx-a stx-b)
                                             (attach-info 
                                              (if (eq? so-far nothing-so-far)
                                                  #`(begin #,(recon-source-current-marks #`stx-a) #,(recon-source-current-marks #`stx-b))
                                                  #`(begin #,so-far #,(recon-source-current-marks #`stx-b))))]
                                            
                                            #;[(begin clause)
                                             (attach-info
                                              (if (eq? so-far nothing-so-far)
                                                  #`(begin #,(recon-source-current-marks (syntax clause)))
                                                  (error 
                                                   'recon-inner
                                                   "stepper:reconstruct: one-clause begin appeared as context: ~a" (syntax->datum exp)))
                                              exp)]
                                            
                                            #;[(begin)
                                             (attach-info
                                              (if (eq? so-far nothing-so-far)
                                                  #`(begin)
                                                  (error 
                                                   'recon-inner
                                                   "stepper-reconstruct: zero-clause begin appeared as context: ~a" (syntax->datum exp))))]
                                            
                                            ; begin0 : 
                                            ;; one-body begin0: perhaps this will turn out to be a special case of the
                                            ;; many-body case.
                                            [(begin0 body)
                                             (if (eq? so-far nothing-so-far)
                                                 (recon-source-current-marks exp)
                                                 (error 'recon-inner "one-body begin0 given as context: ~a" exp))]
                                            
                                            ;; the only time begin0 shows up other than in a fake-exp is when the first 
                                            ;; term is being evaluated
                                            [(begin0 first-body . rest-bodies)
                                             (if (eq? so-far nothing-so-far)
                                                 (error 'foo "not implemented")
                                                 ;; don't know what goes here yet
                                                 #`(begin0 #,so-far #,@(map recon-source-current-marks (syntax->list #`rest-bodies))))]
                                            
                                            ; let-values
                                            
                                            [(let-values . rest) (recon-let)]
                                            
                                            [(letrec-values . rest) (recon-let)]
                                            
                                            [(set! var rhs)
                                             (begin
                                               (when (eq? so-far nothing-so-far)
                                                 (error 'reconstruct "breakpoint before an if reduction should have a result value"))
                                               (attach-info
                                                (let ([rendered-var (reconstruct-set!-var mark-list #`var)])
                                                  #`(set! #,rendered-var #,so-far))
                                                exp))]
                                            
                                            ; lambda : there is no break on a lambda
                                            
                                            [else
                                             (error
                                              'recon-inner
                                              "stepper:reconstruct: unknown object to reconstruct: ~a" (syntax->datum exp))]))))
         
         ; the main recursive reconstruction loop is in recon:
         ; recon : (syntax mark-list boolean -> syntax)
         
         (define (recon so-far mark-list first)
           (cond [(null? mark-list) ; now taken to indicate a callback:
                  (unless (eq? so-far nothing-so-far)
                    (set! last-so-far so-far))
                  so-far
                  ;(error `recon "expcted a top-level mark at the end of the mark list.")
                  ]
                 [else
                  (case (mark-label (car mark-list)) 
                    [(top-level)
                     (if (null? (cdr mark-list))
                         (reconstruct-top-level (mark-source (car mark-list)) so-far)
                         (error 'recon "top-level-define mark found at non-end of mark list"))]
                    [else
                     (let ([reconstructed (recon-inner mark-list so-far)])
                       (recon
                        (if first
                            (mark-as-highlight reconstructed)
                            ; this situation should only happen in lazy racket, 
                            ; when there are infinite lists -- sometimes
                            ; the so-far doesnt get used, so we lose the highlight
                            (if (no-highlight? reconstructed)
                                (mark-as-highlight reconstructed)
                                reconstructed))
                        (cdr mark-list)
                        #f))])]))

         ; uncomment to see all breaks coming in:
         #;(define _ (printf "break-kind: ~a\ninnermost source: ~a\nreturned-value-list: ~a\n" 
                           break-kind
                           (and (pair? mark-list)
                                (syntax->datum (mark-source (car mark-list))))
                           returned-value-list))
           


         
         (define answer
           (begin
             (set! partially-evaluated-promises-table (make-weak-hash))
             (set! last-so-far null)
             (case break-kind
             ((left-side)
              (let* ([innermost 
                      (if returned-value-list ; is it a normal-break/values?
                          (begin 
                            (unless 
                                (and (pair? returned-value-list) 
                                     (null? (cdr returned-value-list)))
                              (error 'reconstruct "context expected one value, given ~v" returned-value-list))
                            (recon-value (car returned-value-list) render-settings))
                          nothing-so-far)])
                (recon innermost mark-list #t)))
             ((right-side)
              (let* ([innermost 
                      (if returned-value-list ; is it an expr -> value reduction?
                          (begin 
                            (unless 
                                (and (pair? returned-value-list) 
                                     (null? (cdr returned-value-list)))
                              (error 'reconstruct "context expected one value, given ~v" returned-value-list))
                            (recon-value (car returned-value-list) render-settings))
                          (recon-source-expr (mark-source (car mark-list)) mark-list null null render-settings))])
                (recon (mark-as-highlight innermost) (cdr mark-list) #f)))
             ((double-break)
              (let* ([source-expr (mark-source (car mark-list))]
                     [innermost-before 
                      (mark-as-highlight (recon-source-expr source-expr mark-list null null render-settings))]
                     [newly-lifted-bindings 
                      (syntax-case source-expr (letrec-values)
                        [(letrec-values ([vars . rest] ...) . bodies)
                         (apply append (map syntax->list (syntax->list #`(vars ...))))]
                        [(let-values ([vars . rest] ...) . bodies)
                         (apply append (map syntax->list (syntax->list #`(vars ...))))]
                        [else (error 'reconstruct "expected a let-values as source for a double-break, got: ~e"
                                     (syntax->datum source-expr))])]
                     [innermost-after 
                      (mark-as-highlight 
                       (recon-source-expr (mark-source (car mark-list)) mark-list null newly-lifted-bindings render-settings))])
                (list (recon innermost-before (cdr mark-list) #f)
                      (recon innermost-after (cdr mark-list) #f))))) ) )
         
         )
      
      answer))
  
  
  
  )
