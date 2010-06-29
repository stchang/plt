(module test mzscheme

(define-syntax (defsubst-process stx)
    (syntax-case stx ()
      [(_ name (acc ...))
       #'(define-syntax (name stx)
           (syntax-case stx () acc ...))]
      [(_ name (acc ...) id subst . more) (identifier? #'id)
       #'(defsubst-process
           name (acc ...
                 (id (identifier? #'id) #'subst)
                 ((id x (... ...)) #'(subst x (... ...))))
           . more)]
      [(_ name (acc ...) n+a subst . more)
       #'(defsubst-process name (acc ... (n+a #'subst)) . more)]))
  (define-syntax defsubst
    (syntax-rules ()
      [(_ (name . args) subst . more)
       (defsubst-process name () (name . args) subst . more)]
      [(_ name subst . more)
       (defsubst-process name () name subst . more)]))

  (define-values (lazy-proc lazy-proc?)
  (let-values ([(type make pred ref set)
                (make-struct-type
                 'lazy-proc #f 1 0 #f null (current-inspector) 0)])
    (values make pred)))

    (define-syntax ~begin
    (let ([ids (syntax->list
                #'(~define ~define-values define-syntax define-syntaxes
                   define-struct require provide))])
      (define (definition? stx)
        (ormap (lambda (id) (module-identifier=? id stx)) ids))
      (lambda (stx)
        (syntax-case stx ()
          ;; optimize simple cases
          [(_) #'(begin)]
          [(_ expr) #'expr]
          [(_ expr ...)
           (let loop ([exprs #'(expr ...)] [defs '()])
             (syntax-case exprs ()
               [((head . rest) expr ...)
                (definition? #'head)
                (loop #'(expr ...) (cons #'(head . rest) defs))]
               ;; only definitions
               [() #`(begin #,@(reverse defs))]
               ;; single expr
               [(expr) #`(begin #,@(reverse defs) expr)]
               [(expr ...)
                #`(begin #,@(reverse defs) (~ (begin (! expr) ...)))]))]))))

  ;; redefined to use lazy-proc and ~begin
  (define-syntax (~lambda stx)
    (syntax-case stx ()
      [(_ args body0 body ...)
       (let ([n (syntax-local-name)])
         (with-syntax ([lam (syntax-property
                             (syntax/loc stx
                               (lambda args (~begin body0 body ...)))
                             'inferred-name n)])
           (syntax/loc stx (lazy-proc lam))))]))
  (defsubst
    (~define (f . xs) body0 body ...) (define f (~lambda xs body0 body ...))
    (~define v x) (define v x))
  
  (~define (f x) (+ x 1))
  
  )