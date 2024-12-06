#lang racket

(define env (hash '+ + '- - '/ / '* * '= = '<= <= '< < '>= >= '> >))

(define (mainStartEval expr Table)
  (cond
    [(number? expr) expr]
    [(pair? expr)
     (letrec ([op (tableOperator? (car expr) Table)]
              [args (cdr expr)])
       (cond
         [(number? op) op]

         [(hash-has-key? env op)
           ((hash-ref env op) (mainStartEval (car args) Table) (mainStartEval (car (cdr args)) Table))]

         
         [(equal? op 'if)
          (if (mainStartEval (car args) Table)
               (mainStartEval (cadr args) Table)
               (mainStartEval (caddr args) Table))]

         ;; For constants for variables we will look up in namespace
         [(equal? op 'quote) (car args)]

         [(equal? op 'car) (let ([variable (mainStartEval (cadr expr) Table)])
                             (if (list? variable) (car variable)
                                 variable
                                 )
                             )]

         [(equal? op 'cdr) 
 (let ([variable (mainStartEval (cadr expr) Table)])
   (if (list? variable) 
       (cdr variable)
       variable))]


         [(equal? op 'pair?) (pair? (mainStartEval (cadr expr) Table))]

         [(equal? op 'cons)
               (let ([arg1 (mainStartEval (car args) Table)]
                     [arg2 (mainStartEval (cadr args) Table)])
                     (cons arg1 arg2))]
         
         [(equal? op 'lambda)
                          (if (null? (cdddr expr)) (printf "~a\n" 'lambda)
                          (mainStartEval (caddr expr) (extendingTable Table  (cadr expr) (list (cadddr expr)))))
          ]
          ;;(mainStartEval (car (cdr args)) extendingTable Table (car args)))]
               ;; CADR EXPR = ARGS and CADDR EXPR = BODY

        [(equal? op 'let) (mainStartEval (car (cdr args)) (append (car args) Table))]

        [(equal? op 'letrec) (mainStartEval (car (cdr args)) (append (car args) Table))]

        ;; car op == operation
        ;; cadr == formal params
        ;;
        ;; two scenarios caar expr nested lambda
        ;; else simple lambda

        #|
          (mainStartEval (car (cdr (cdr (car (car expr))))) (extendingTable Table (cadaar expr) (cdr expr)) )
|#
        [(list? op) (if (equal? op (car expr)) 
         (if (pair? (caar expr))
          (mainStartEval (append (car (cdr (cdr (car (car expr))))) (cdar expr)) (extendingTable Table (cadaar expr) (cdr expr)) )
          (mainStartEval (caddar expr) (extendingTable Table (cadar expr) (cdr expr)))
          )
         (mainStartEval (append op (list (cadr expr))) Table))
         ]
      

        [else (error "Something wrong")]))]
    [else (tableOperator? expr Table)]))

(define (tableOperator? op Table)
  (cond
    [(null? Table) op] ; If the list is empty, return op
    [(pair? (car Table)) ; If the current element is a pair (like '(x 2))
     (if (equal? (caar Table) op) ; Compare the car of the car with the op
         (let ([value (car (cdar Table))]) ; Get the value from the pair
           (if (and (pair? value) (equal? (car value) 'lambda)) ; Check if it's a lambda
               (list value) ; Return the lambda function as-is
               (mainStartEval value Table))) ; Otherwise, evaluate the value
         (tableOperator? op (cdr Table)))] ; Recurse to the next element
    [else (tableOperator? op (cdr Table))])) ; Else, continue searching


(define (extendingTable Table formalParams actualParams)
  (if (null? formalParams)
      Table
      (extendingTable 
       (append (list (list (car formalParams) (mainStartEval (car actualParams) Table))) 
               Table)
       (cdr formalParams)
       (cdr actualParams))))


(define (startEval sourceCode) (mainStartEval sourceCode '()))



(startEval '(let ([+ *]) (+ 5 5)))

(startEval '(letrec ([y 1] [x 2]) (+ x y)))

(startEval '(let ([x 5])
(let ([y 10])
    (+ x y)))
)

(startEval '((lambda (x y) (+ x y)) 1 3))

(startEval
  '(letrec ((fib
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
           (fib 7))
 )

(startEval
  '(let ((y 10))
     (let ((f (lambda (x) (+ x y))))
       (let ((y 100))
         (f 2)))))

(startEval '(let ((+ (lambda (x) (cdr x)))
                   (- '(1 2 3 4 5)))
               (+ -))
 )
(startEval '(letrec ((x (- 9 5))(e (* 3 1))) (* e 5)))
(startEval '(lambda (x) (+ x 1)))

(startEval '(((lambda (x) (lambda (y) (+ x y)))1)2))
(print
(startEval
'(letrec ((fact
(lambda (x)
(if (= x 0) (quote 1)
(* x (fact (- x 1)))))))
(fact 10))))

