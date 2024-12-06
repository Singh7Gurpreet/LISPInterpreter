#lang racket

;; Define the hash table with built-in functions
(define built-in-functions
  (hash '+ + '- - '* * '/ / '= = '<= <= '< < '>= >= '> > 'equal? equal?))

;; The main evaluation function
(define (mainStartEval expr env)
  (cond
    ;; Numbers and booleans evaluate to themselves
    [(or (number? expr) (boolean? expr)) expr]

    ;; Variables: look them up in the environment
    [(symbol? expr) (lookup-variable expr env)]

    ;; Lists: evaluate expressions
    [(pair? expr)
     (let ([op (car expr)]
           [args (cdr expr)])
       (cond
         ;; Special forms
         [(and (symbol? op)
               (or (equal? op 'if)
                   (equal? op 'quote)
                   (equal? op 'lambda)
                   (equal? op 'let)
                   (equal? op 'letrec)))
          (case op
            [(if)
             (let ([test (mainStartEval (car args) env)])
               (if test
                   (mainStartEval (cadr args) env)
                   (mainStartEval (caddr args) env)))]
            [(quote) (cadr expr)]
            [(lambda)
             ;; Create a closure with parameters, body, and environment
             (list 'closure (car args) (cadr args) env)]
            [(let)
             (let* ([bindings (car args)]
                    [body (cadr args)]
                    [new-env (extend-env-let bindings env)])
               (mainStartEval body new-env))]
            [(letrec)
             (let* ([bindings (car args)]
                    [body (cadr args)]
                    [new-env (extend-env-letrec bindings env)])
               (mainStartEval body new-env))])]
         ;; Otherwise, evaluate the operator and apply it
         [else
          (let ([op-evaluated (mainStartEval op env)]
                [arg-values (map (lambda (arg) (mainStartEval arg env)) args)])
            (apply-function op-evaluated arg-values env))]))]

    ;; Error for unsupported expressions
    [else (error "Unsupported expression" expr)]))

;; Function to apply functions and closures
(define (apply-function func arg-values env)
  (cond
    ;; If the function is a built-in procedure
    [(procedure? func)
     (apply func arg-values)]
    ;; If the function is a closure
    [(and (pair? func) (equal? (car func) 'closure))
     (let* ([params (cadr func)]
            [body (caddr func)]
            [closure-env (cadddr func)]
            [new-env (extend-env params arg-values closure-env)])
       (mainStartEval body new-env))]
    ;; Error if not a function
    [else (error "Attempted to call a non-function" func)]))

;; Variable lookup
(define (lookup-variable var env)
  (cond
    [(null? env)
     (if (hash-has-key? built-in-functions var)
         (hash-ref built-in-functions var)
         (error "Unbound variable" var))]
    [(equal? (caar env) var)
     (let ([value (cdar env)])
       (if (promise? value)
           (force value)
           value))]
    [else (lookup-variable var (cdr env))]))

;; Extend environment for function application
(define (extend-env params args env)
  (if (null? params)
      env
      (extend-env (cdr params) (cdr args)
                  (cons (cons (car params) (car args)) env))))

;; Extend environment for 'let'
(define (extend-env-let bindings env)
  (if (null? bindings)
      env
      (let* ([var (caar bindings)]
             [expr (cadar bindings)]
             [value (mainStartEval expr env)])
        (extend-env-let (cdr bindings)
                        (cons (cons var value) env)))))

;; Extend environment for 'letrec' without mutation
(define (extend-env-letrec bindings env)
  (letrec ([rec-env (append (map (lambda (binding)
                                   (cons (car binding)
                                         (delay (mainStartEval (cadr binding) rec-env))))
                                 bindings)
                            env)])
    rec-env))

;; Wrapper function to start evaluation with the initial environment
(define (startEval sourceCode)
  (mainStartEval sourceCode '()))

;; =====================
;; Test Cases
;; =====================

;; Test Case 1: Simple let binding
(displayln "Test A: (let ([+ *]) (+ 5 5))")
(displayln (startEval '(let ([+ *]) (+ 5 5)))) ; Expected: 25
(newline)

;; Test Case 2: letrec with factorial
(displayln "Test B: (letrec ((fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))) (fact 10))")
(displayln (startEval
           '(letrec ((fact
                      (lambda (x)
                        (if (= x 0) 1
                            (* x (fact (- x 1)))))))
              (fact 10)))) ; Expected: 3628800
(newline)

;; Test Case 3: let with nested let
(displayln "Test C: (let ([x 5]) (let ([y 10]) (+ x y)))")
(displayln (startEval
           '(let ([x 5])
              (let ([y 10])
                  (+ x y))))) ; Expected: 15
(newline)

;; Test Case 4: Lambda application
(displayln "Test D: ((lambda (x y) (+ x y)) 1 3)")
(displayln (startEval '((lambda (x y) (+ x y)) 1 3))) ; Expected: 4
(newline)

;; Test Case 5: Nested lambdas
(displayln "Test E: (((lambda (x) (lambda (y) (+ x y))) 1) 2)")
(displayln (startEval '(((lambda (x) (lambda (y) (+ x y))) 1) 2))) ; Expected: 3
(newline)

;; Test Case 6: Additional test with higher-order functions
(displayln "Test F: (let ([add (lambda (x) (lambda (y) (+ x y)))]) ((add 5) 10))")
(displayln (startEval
           '(let ([add (lambda (x) (lambda (y) (+ x y)))])
              ((add 5) 10)))) ; Expected: 15
(newline)

;; Test Case 7: Boolean and conditionals
(displayln "Test G: (if #t 42 0)")
(displayln (startEval '(if #t 42 0))) ; Expected: 42
(newline)

;; Test Case 8: Closure capturing environment
(displayln "Test H: (let ([x 10]) ((lambda (y) (+ x y)) 5))")
(displayln (startEval '(let ([x 10]) ((lambda (y) (+ x y)) 5)))) ; Expected: 15
(newline)

;; Test Case 9: Variable shadowing in nested lets
(displayln "Test I: (let ([x 42]) (let ([x 100]) x))")
(displayln (startEval '(let ([x 42]) (let ([x 100]) x)))) ; Expected: 100
(newline)

;; Test Case 10: Complex arithmetic expression
(displayln "Test J: (* (+ 1 2) (- 10 4))")
(displayln (startEval '(* (+ 1 2) (- 10 4)))) ; Expected: 18
(newline)

;; Test Case 11: Unbound variable (should produce an error)
(displayln "Test K: Unbound variable error scenario (e.g., x)")
(with-handlers ([exn:fail?
                 (lambda (e) (displayln (string-append "Error: " (exn-message e))))])
  (displayln (startEval 'x))) ; Expected: Error (Unbound variable)
(newline)
