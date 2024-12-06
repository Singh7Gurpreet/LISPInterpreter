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

;; Test 7: is-odd? Function
(print "Test G: ")
(print
 (startEval '(let ([sub1 (lambda (x) (- x 1))]
                    [not (lambda (x) (if x #f #t))])
                (letrec ([is-even? (lambda (n)
                                     (if (= n 0)
                                         #t
                                         (is-odd? (sub1 n))))]
                         [is-odd? (lambda (n)
                                    (if (not (= n 0))
                                        (is-even? (sub1 n))
                                        #f))])
                  (is-odd? 4))))
)

(newline)
(print "Expected: #f")
(newline)

;; Test 7: Closure Environment
(print "Test H: ")
(print
 (startEval
  '(let ((y 5))
     (let ((f (lambda (x) (+ x y))))
       (let ((y 50))
         (f 3)))))
)
(newline)
(print "Expected: 8")
(newline)

;; Test 8: Increment Function
(print "Test 8: ")
(print
 (startEval
  '(let ((inc (lambda (x) (+ x (quote 1)))))
     (inc (quote 10)))
 )
)
(newline)
(print "Expected: 11") 
(newline)

;; Test 9: Fibonacci Function
(print "Test I: ")
(print
 (startEval
  '(letrec ((fib
             (lambda (n)
               (if (< n 2)
                   n
                   (+ (fib (- n 1)) (fib (- n 2)))))))
         (fib 6)))
)
(newline)
(print "Expected: 8") 
(newline)

;; Test 10: Nested Lambda Application
(print "Test J: ")
(print
 (startEval
  '(((lambda (x) (lambda (y) (+ x y))) 3) 4))
)
(newline)
(print "Expected: 7")
(newline)

;; Test 11: Equality Check
(print "Test K: ")
(print
 (startEval
  '(let ((x (+ 2 2))) 
     (equal? x 4))
 )
)
(newline)
(print "Expected: #t")
(newline)

;; Test 12: Variable Shadowing
(print "Test L: ")
(print
 (startEval
  '(let ((y 7) (x 4)) 
     (let ((x 9)) 
       (+ x y))))
)
(newline)
(print "Expected: 16")
(newline)


;; Test 13 Increment Function with Different Operator
(print "Test M: ")
(print
 (startEval
  '(let ((inc (lambda (x) (+ x (quote 2)))))
     (inc (quote 3)))
 )
)
(newline)
(print "Expected: 5")
(newline)

;; Test 14: Simple Addition
(print "Test N: ")
(print
 (startEval '(+ (quote 7) (quote 2)))
)
(newline)
(print "Expected: 9")
(newline)

;; Test 15: Letrec with Multiple Bindings
(print "Test O: ")
(print
 (startEval 
  '(letrec ((x (- 10 6))
            (e (* 2 5))) 
     (* e 5)))
)
(newline)
(print "Expected: 50")
(newline)

;; Test 16: Lambda with Letrec
(print "Test P: ")
(print
 (startEval 
  '(letrec ((y 3)
            (f (lambda (x) (+ x y)))) 
     (f 4)))
)
(newline)
(print "Expected: 7")
(newline)