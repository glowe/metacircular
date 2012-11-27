;; Define these for compatibility between various schemes
(define true #t)
(define false #f)

(load "stdlib.scm")  ;; Load the standard library of routines (e.g., append, null?, map, length, etc.)

;;
;; +-------+
;; | UTILS |
;; +-------+
;;

;; Returns a procedure that tests whether an expression is a list
;; whose first element is the symbol denoted by tag.
(define (tagged-list? tag)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) tag))))

;; Tests whether an expression is a dotted pair (i.e., (1 . 2)).
(define (dotted-pair? exp)
  (and (pair? exp)
       (not (list? exp))))

;;
;; +------+
;; | EVAL |
;; +------+
;;

(define (eval exp env)
  ;; NOTE: The order of these conditionals is important
  (cond ((literal? exp) (eval-literal exp env))
        ((variable? exp) (eval-variable exp env))
        ((quoted? exp) (eval-quoted exp env))
        ((if? exp) (eval-if exp env))
        ((cond? exp) (eval-cond exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((begin? exp) (eval-begin exp env))
        ((let? exp) (eval-let exp env))
        ((let*? exp) (eval-let* exp env))
        ((letrec? exp) (eval-letrec exp env))
        ((lambda? exp) (eval-lambda exp env))
        ((define? exp) (eval-define exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((define-macro? exp) (eval-define-macro exp env))
        ((application? exp) (eval-application exp env))
        (else (error "unhandled expression type: " exp))))


;;
;; +---------+
;; | LITERAL |
;; +---------+
;;


;; Tests whether an expression is a literal.
(define (literal? exp)
  (or (number? exp)
      (string? exp)
      (null? exp)
      (eq? exp #t)
      (eq? exp #f)))

;; To evaluate a literal, return the literal without
;; translation.
(define (eval-literal exp env) exp)

;;
;; +-------+
;; | QUOTE |
;; +-------+
;;

;; Tests whether an expression is quoted.
(define quoted? (tagged-list? 'quote))

;; To evaluate a quote, return the text of the quotation.
(define (eval-quoted exp env)
  (car (cdr exp)))


;;
;; +------+
;; | VARIABLE |
;; +------+
;;


;; Tests whether an expression is a variable.
(define variable? symbol?)

;; To evaluate a variable, look it up in the environment.
(define (eval-variable exp env)
  (lookup-in-env exp env))


;;
;; +--------+
;; | LAMBDA |
;; +--------+
;;

;; Tests whether an expression is a lambda expression.
(define lambda? (tagged-list? 'lambda))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;; Extracts a lambda's parameters.
(define (lambda-params exp)
  (if (missing-lambda-params? exp)
      (error "bad lambda syntax (missing params) in:" exp)
      (car (cdr exp))))

;; Extracts a lambda's body.
(define (lambda-body exp)
  (if (or (missing-lambda-params? exp)
          (missing-lambda-body? exp))
      (error "bad lambda syntax (missing body) in:" exp)
      (cdr (cdr exp))))

(define (missing-lambda-params? exp)
  (null? (cdr exp)))

(define (missing-lambda-body? exp)
  (null? (cdr (cdr exp))))

;;
;; Evaluates a lambda expression
;;
;; To evaluate a lambda expression, we create a closure.
(define (eval-lambda exp env)
  (let ((params (lambda-params exp))
        (body (lambda-body exp)))
    (make-closure params body env)))

;;
;; +----------+
;; | CLOSURES |
;; +----------+
;;


;; Tests whether an expression is a closure
(define (closure? exp)
  (equal? (car exp) "#<closure>"))

;; Since we can only create closures using make-closure, error
;; checking is unnecessary.
(define (closure-params exp)
  (car (cdr exp)))

(define (closure-body exp)
  (car (cdr (cdr exp))))

(define (closure-env exp)
  (car (cdr (cdr (cdr exp)))))

(define (make-closure params body env)
  (list "#<closure>" params body env))


;;
;; +----+
;; | IF |
;; +----+
;;

;; Tests whether an expression is an if expression.
(define if? (tagged-list? 'if))

;; Evaluates an if expression:
;;
;; 1.) Evaluate the predicate.
;; 2.) If the predicate was true, evaluate the consequent;
;; otherwise, evaluate the alternative.
(define (eval-if exp env)
  (if (eval (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Accessors

(define (if-predicate exp)
  (if (missing-if-predicate? exp)
      (error "bad if syntax (i.e., missing predicate) in:" exp)
      (car (cdr exp))))

(define (if-consequent exp)
  (if (or (missing-if-predicate? exp)
          (missing-if-consequent? exp))
      (error "bad if syntax (i.e., missing consequent) in:" exp)
  (car (cdr (cdr exp)))))

(define (if-alternative exp)
  (if (missing-if-alternative? exp)
      (error "bad if syntax (i.e., missing alternative) in:" exp)
      (car (cdr (cdr (cdr exp))))))

(define (missing-if-predicate? exp)
  (null? (cdr exp)))

(define (missing-if-consequent? exp)
  (null? (cdr (cdr exp))))

(define (missing-if-alternative? exp)
  (null? (cdr (cdr (cdr exp)))))


;;
;; +--------+
;; | AND/OR |
;; +--------+
;;
;; We don't directly evaluate "and"s or "or"s. They are special
;; forms since their arguments require normal order evaluation
;; similar to ifs. Therefore, we merely transform these expression
;; types into their if equivalents.
;;


(define and? (tagged-list? 'and))


(define (and a b . c)
  (define (helper booleans)
    (cond ((null? booleans) true)
          ((not (car booleans)) false)
          (else (helper (cdr booleans)))))
  (if (not a)
      false
      (if (not b)
          false
          (helper c))))

(define (eval-and exp env)
  (eval (and->if exp) env))


;;
;; (and a (b) c)
;;
;; should be transformed to
;;
;; (if (not a)
;;     false
;;     (if (not (b))
;;         false
;;         (if (not c)
;;             false
;;             true)))
;;

(define (and->if exp)
  (define (helper predicates)
    (if (null? predicates)
        'true
        (list 'if (list 'not (car predicates)) 'false (helper (cdr predicates)))))
  (helper (cdr exp)))


(define or? (tagged-list? 'or))

(define (eval-or exp env)
  (eval (or->if exp) env))

;;
;; (or a (b) c)
;;
;; should be transformed to
;;
;; (if a
;;     true
;;     (if (b)
;;         true
;;         (if c
;;             true
;;             false)))
;;
(define (or->if exp)
  (define (helper predicates)
    (if (null? predicates)
        'false
        (list 'if (car predicates) 'true (helper (cdr predicates)))))
  (helper (cdr exp)))

;;
;; +------+
;; | COND |
;; +------+
;;

;; Tests whether an expression is a conditional.
(define cond? (tagged-list? 'cond))

;; Evaluates a conditional expression.
(define (eval-cond exp env)
  (eval (cond->if exp) env))


;;
;; cond->if
;;
;; Transform a cond expression into an if expression.
;;
;; For example:
;;
;;  (cond (true foo)
;;        ((bar? bar) (do-something))
;;        (else (do-something-else)))))
;;
;;  ...should be transformed to...
;;  (if true
;;      foo
;;      (if (bar? bar)
;;          (do-something)
;;          (do-something-else)))
;;
;; Because a cond clause may contain a sequence of expressions as
;; its consequent, the tranformation should wrap multi-expression
;; consequents with "begin". For example:
;;
;; (cond (true (foo) (bar))
;;       (other-true baz)
;;       (else (do-something-else)))))))
;;
;; ...should be transformed to...
;;
;; (if true
;;     (begin (foo)
;;            (bar))
;;     (if other-true
;;         baz
;;         (do-something-else)))
;;
(define (cond->if exp)
  (define (helper e)
    (if (null? e) '()
        (let ((clause (cond-clause e)))
          (let ((predicate (cond-predicate clause))
                (consequents (cond-consequents clause))
                (rest (cdr e)))
            (if (eq? predicate 'else)
                (if (not (null? rest))
                    (error "bad cond syntax (i.e., else not last clause) in " exp)
                    (make-begin consequents))
                (list 'if predicate (make-begin consequents) (helper rest)))))))
  ;; Using a helper procedure allows us to skip the cond part of
  ;; the expression.
  (helper (cdr exp)))

;; Accessors

(define (cond-clause exp)
  (car exp))

(define (cond-predicate exp)
  (if (missing-cond-predicate? exp)
      (error "bad cond syntax (i.e., missing predicate) in:" exp)
      (car exp)))

(define (cond-consequents exp)
  (if (or (missing-cond-predicate? exp)
          (missing-cond-consequents? exp))
      (error "bad cond syntax (i.e., missing consequents) in:" exp)
      (cdr exp)))

(define (missing-cond-predicate? exp)
  (null? exp))

(define (missing-cond-consequents? exp)
  (null? (cdr exp)))

;;
;;





;;
;; +-------+
;; | BEGIN |
;; +-------+
;;


;; A begin expression is a sequence of expressions that should be
;; evaluated in order. The value returned when evaluating the
;; sequence should be the value of the last expression in the
;; sequence.

;; Tests whether an expression is a begin expression.
(define begin? (tagged-list? 'begin))

(define (eval-begin exp env)
  (eval-sequence (begin-body exp) env))

(define (eval-sequence exp env)
  (if (null? (cdr exp)) (eval (car exp) env)
      (begin (eval (car exp) env)
             (eval-sequence (cdr exp) env))))

(define (begin-body exp)
  (cdr exp))


(define (make-begin body)
  (if (not (pair? body))
      (error "expects a list of expressions, but given:" body)
      ;; Optimization: don't create a begin for a single expression
      (if (null? (cdr body)) (car body)
          (append '(begin) body))))
;;
;; +-----+
;; | LET |
;; +-----+
;;


;; A let expression defines a set of bindings for a temporary
;; scope. These bindings may shadow some existing variables.

;; Tests whether an expression is a let expression.
(define let? (tagged-list? 'let))

(define let*? (tagged-list? 'let*))

(define letrec? (tagged-list? 'letrec))


(define (let-bindings exp)
  (if (missing-let-bindings? exp)
      (error "bad let syntax (missing bindings) in:" exp)
      (car (cdr exp))))

(define (missing-let-bindings? exp)
  (null? (cdr exp)))

(define (let-body exp)
  (if (or (missing-let-bindings? exp)
          (missing-let-body? exp))
      (error "bad let syntax (missing body) in:" exp)
      (cdr (cdr exp))))

(define (missing-let-body? exp)
  (null? (cdr (cdr exp))))

;; Evaluates a let expression.
;; Evaluation involves the following steps:

;; 1.) We extend the current environment with the new set of
;; let bindings.
;; 2.) We evaluate the let body in this new environment (the let
;; body may contain a sequence of expressions, so we treat it like
;; a begin block).
;; 3.) We return the evaluated expression and the original
;; environment.
(define (eval-let exp env)
  (let ((bindings (let-bindings exp))
        (body (let-body exp)))
    (eval-sequence body
          (extend-env
           (map (lambda (binding)
                  (make-binding (car binding)
                                (eval (car (cdr binding)) env)))
                bindings)
           env))))

(define (eval-let* exp env)
  (eval (let*->let exp) env))


;;
;; (let* ((a 1) (b a) (c b)) (+ a b c))
;;
;; ->
;;
;; (let ((a 1)) (let ((b a)) (let ((c b)) (+ a b c))))
;;
(define (let*->let exp)
  (define (helper bindings body)
    (if (not (null? bindings))
        (list (cons 'let (cons (list (car bindings)) (helper (cdr bindings) body))))
        body))
  (let ((bindings (let-bindings exp))
        (body (let-body exp)))
    (cons 'let (cons (list (car bindings)) (helper (cdr bindings) body)))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))


(define (make-let bindings body)
  (cons 'let (cons bindings body)))

;;
;; (letrec ((a 1) (b 1)) (+ a b))
;;
;; ->
;; (let ((a *-dummy-value-*)
;;       (b *-dummy-value-*))
;;   (set! a 1)
;;   (set! b 1)
;;   (+ a b))
;;
;; (let ((binding-variable-1 dummy-value))
;;      ((binding-variable-2 dummy-value))
;;   (set! binding-variable-1 binding-value-1)
;;   (set! binding-variable-2 binding-value-2)
;;   (let-body exp)
;;

(define (letrec->let exp)
  (let ((bindings (let-bindings exp)))
    (make-let (map (lambda (binding)
                     (make-binding (binding-variable binding)
                                   '*-dummy-value-*))
                   bindings)
              (append (map (lambda (binding)
                             (cons 'set!
                                   (cons (binding-variable binding)
                                         (binding-value binding))))
                           bindings)
                      (let-body exp)))))





;;
;; +------------+
;; | ASSIGNMENT |
;; +------------+
;;

(define assignment? (tagged-list? 'set!))

(define (assignment-variable exp)
  (car (cdr exp)))

(define (assignment-value exp)
  (car (cdr (cdr exp))))

(define (eval-assignment exp env)
  (set-in-frame!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   (first-frame env)))

;;
;; +--------+
;; | DEFINE |
;; +--------+
;;

;;
;; A normal define has the following structure:
;;
;; (define a b) (keyword variable exp)
;;
;; There's also a shortcut version for defining procedures:
;;
;; (define (foo bar baz)
;;    (+ bar baz))
;;
;; (keyword (variable formal-param*)
;;     body-exp*)
;;

(define define? (tagged-list? 'define))

(define (define-procedure? exp)
  (pair? (car (cdr exp))))

(define (define-variable exp)
  (if (define-procedure? exp)
      (car (car (cdr exp)))
      (car (cdr exp))))

(define (define-value exp)
  (if (define-procedure? exp)
      (make-lambda (cdr (car (cdr exp))) (cdr (cdr exp)))
      (car (cdr (cdr exp)))))

(define (eval-define exp env)
  (define-in-env! (define-variable exp)
                  (eval (define-value exp) env)
                  env))

;;
;; +--------+
;; | MACROS |
;; +--------+


(define define-macro? (tagged-list? 'define-macro))

(define (define-macro-variable exp)
  (car (car (cdr exp))))

(define (define-macro-params exp)
  (cdr (car (cdr exp))))

(define (define-macro-body exp)
  (cdr (cdr exp)))


(define (eval-define-macro exp env)
  (define-in-env!
    (define-macro-variable exp)
    (make-macro (define-macro-params exp) (define-macro-body exp))
    env))

(define (make-macro params body)
  (cons "#<macro>" (cons params body)))

(define (macro-params macro)
  (car (cdr macro)))

(define (macro-body macro)
  (cdr (cdr macro)))

(define (macro? exp)
  (equal? (car exp) "#<macro>"))

;;
;; +-----------------------+
;; | PROCEDURE-APPLICATION |
;; +-----------------------+
;;

;; Tests whether an expression is a procedure application
(define (application? exp)
  (pair? exp))

(define (application-op exp)
  (car exp))

(define (application-args exp)
  (cdr exp))

;; Tests whether an expression is a primitive procedure
(define (primitive-procedure? exp)
  (equal? (car exp) "#<primitive-procedure>"))


(define (make-primitive-procedure impl)
  (list "#<primitive-procedure>" impl))

(define (primitive-impl primitive)
  (car (cdr primitive)))

(define (add-primitive-procedure-to-env variable impl env)
  (define-in-env!
    variable
    (make-primitive-procedure impl)
    env))

(define (eval-application exp env)
  (let ((op (eval (application-op exp) env))
        (args (application-args exp)))
    (if (macro? op)
        (apply-macro op args env)
        (my-apply op (eval-args args env)))))

(define (eval-args args env)
  (cond ((null? args) '())
        ((not (pair? args)) (eval args env))
        (else
         (cons (eval (car args) env)
               (eval-args (cdr args) env)))))


;;
;; +-------+
;; | APPLY |
;; +-------+
;;

(define (my-apply procedure args)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-impl procedure) args))
        (else (apply-compound-procedure procedure args))))



;; To apply a compound procedure we extend the procedure's
;; environment with the procedure's arguments and evaluate its
;; body.
;;
(define (apply-compound-procedure procedure args)
  (let ((params (closure-params procedure))
        (env (closure-env procedure)))
    (eval-sequence
     (closure-body procedure)
     (extend-env
      (if (or (dotted-pair? params)
              (and (not (pair? params))
                   (not (null? params))))
          (make-dotted-pair-frame params args)
          (make-frame params args))
      env))))

(define (apply-macro procedure args env)
  (eval (eval-sequence
         (macro-body procedure)
         (extend-env
          (make-frame (macro-params procedure) args)
          env))
        env))

;;
;; +-------------+
;; | ENVIRONMENT |
;; +-------------+
;


;; A frame is a list of binding.
(define (make-frame variables values)
  (define (helper ns vs)
    (if (null? ns)
        '()
        (append (list (make-binding (car ns) (car vs)))
                (make-frame (cdr ns) (cdr vs)))))
  (if (not (= (length variables) (length values)))
      (error "my variables and values mismatch:" variables values)
      (helper variables values)))


(define (make-dotted-pair-frame variables values)
  (cond ((not (pair? variables))
         (list (make-binding variables values)))
        ((and (not (pair? (cdr variables)))
              (not (null? (cdr values))))
         (list (make-binding (car variables) (car values))
               (make-binding (cdr variables) (cdr values))))
        ((not (pair? (cdr variables)))
         (list (make-binding (car variables) (car values))
               (make-binding (cdr variables) '())))
       (else
         (append
          (list (make-binding (car variables) (car values)))
          (make-dotted-pair-frame (cdr variables) (cdr values))))))


(define (first-frame env)
  (car env))

(define (rest-frames env)
  (cdr env))

(define (empty-frame? frame)
  (null? frame))

(define (empty-frame) '())



;; Tests whether variable is in frame.
;; This procedure assumes frame is a valid list
(define (variable-in-frame? variable frame)
  (cond ((empty-frame? frame) #f)
        ((eq? (binding-variable (first-binding frame)) variable) #t)
        (else (variable-in-frame? variable (rest-bindings frame)))))

(define (value-in-frame variable frame)
  (if (null? frame)
      (error "no binding for variable:" variable)
      (let ((binding (first-binding frame)))
        (if (eq? (binding-variable binding) variable)
            (binding-value binding)
            (value-in-frame variable (rest-bindings frame))))))

;; Accessors

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (first-binding frame)
  (car frame))

(define (rest-bindings frame)
  (cdr frame))

;; An environment is a list of frames.

(define (new-env)
  (list (empty-frame)))

;; Looks up a variable in the environment.

(define (lookup-in-env variable env)
  (if (empty-frame? env)
      (error "can't find binding for variable:" variable)
      (let ((frame (first-frame env)))
        (if (variable-in-frame? variable frame)
            (value-in-frame variable frame)
            (lookup-in-env variable (rest-frames env))))))

(define (extend-env frame env)
  (cons frame env))

(define (set-in-frame! variable value frame)
  (let ((binding (first-binding frame)))
    (if (eq? variable (binding-variable binding))
        (set-car! frame (cons variable value))
        (set-in-frame! variable value (rest-frames frame)))))

(define (make-define-in-env! variable-in-frame-fn)
  (lambda (variable value env)
    (let ((frame (first-frame env))
          (rest (rest-frames env)))
      (if (variable-in-frame? variable frame)
          (variable-in-frame-fn variable value frame)
          (begin
            (set-cdr! env rest)
            (set-car! env (cons (make-binding variable value) frame)))))))

(define define-in-env!
  (make-define-in-env!
   (lambda (variable value frame)
     (error "variable already defined in frame:" variable))))

(define top-level-define-in-env!
  (make-define-in-env! set-in-frame!))

(define (set-in-env! variable value env)
  (if (empty-frame? (first-frame env))
      (error "Can't find binding for variable:" variable)
      (let ((frame (first-frame env)))
        (if (variable-in-frame? variable frame)
            (set-in-frame! variable value frame)
            (set-in-env! variable value (rest-frames env))))))


;;
;; +--------------------------------+
;; | REPL AND TOP-LEVEL ENVIRONMENT |
;; +--------------------------------+
;;


(define (setup env)
  (add-primitive-procedure-to-env '* * env)
  (add-primitive-procedure-to-env '+ + env)
  (add-primitive-procedure-to-env '- - env)
  (add-primitive-procedure-to-env '/ / env)
  (add-primitive-procedure-to-env '> > env)
  (add-primitive-procedure-to-env '= = env)
  (add-primitive-procedure-to-env 'apply my-apply env)
  (add-primitive-procedure-to-env 'car car env)
  (add-primitive-procedure-to-env 'cdr cdr env)
  (add-primitive-procedure-to-env 'close-input-port close-input-port env)
  (add-primitive-procedure-to-env 'cons cons env)
  (add-primitive-procedure-to-env 'display display env)
  (add-primitive-procedure-to-env 'eof-object?  eof-object? env)
  (add-primitive-procedure-to-env 'eq? eq? env)
  (add-primitive-procedure-to-env 'equal? equal? env)
  (add-primitive-procedure-to-env 'eqv? eqv? env)
  (add-primitive-procedure-to-env 'error error env)
  (add-primitive-procedure-to-env 'load my-load env)
  (add-primitive-procedure-to-env 'newline newline env)
  (add-primitive-procedure-to-env 'number? number? env)
  (add-primitive-procedure-to-env 'open-input-file open-input-file env)
  (add-primitive-procedure-to-env 'pair? pair? env)
  (add-primitive-procedure-to-env 'read read env)
  (add-primitive-procedure-to-env 'set-car! set-car! env)
  (add-primitive-procedure-to-env 'set-cdr! set-cdr! env)
  (add-primitive-procedure-to-env 'string? string? env)
  (add-primitive-procedure-to-env 'symbol? symbol? env)
  (define-in-env! '#t #t env)
  (define-in-env! '#f #f env)
  (define-in-env! 'true #t env)
  (define-in-env! 'false #f env)
  (add-primitive-procedure-to-env 'not not env)
)


(define (my-load filename)
    (load-repl (open-input-file filename)))


(define (load-repl port)
  (let ((exp (read port)))
    (if (eof-object? exp) 'done
        (let ((result (top-eval exp)))
          (load-repl port)))))

(define (top-eval exp)
  (if (pair? exp)
      (if (eq? (car exp) 'define)
          (top-level-define-in-env!
           (define-variable exp)
           (eval (define-value exp) *global-env*)
           *global-env*)
          (eval exp *global-env*))
      (eval exp *global-env*)))

(define *prompt* "]=> ")

(define (repl)
  (display (call-with-current-continuation
            (lambda (cont)
              (top-level-define-in-env!
               'error
               (make-primitive-procedure cont)
               *global-env*)
              "")))
  (newline)
  (display *prompt*)
  (let ((exp (read)))
    (cond ((or (eof-object? exp)
               (equal? exp '(exit))) 'done)
          (else (display (top-eval exp))
                (newline)
                (repl)))))


(define (display-args args)
  (if (null? args)
      (newline)
      (begin
        (display " ")
        (display (car args))
        (display-args (cdr args)))))

(define *global-env* (new-env))
(setup *global-env*)
