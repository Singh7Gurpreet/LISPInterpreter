#lang racket

(define env (hash '+ + '- - '/ / '* * '= = '<= <= '< < '>= >= '> >))

(define (mainStartEval expr Table)
  (cond
    [(number? expr) expr]
    [(pair? expr)
     (letrec ([op (tableOperator? (car expr) Table)]
              [args (cdr expr)])
       (cond
         ;;(print expr)
         ;; For evaluating nested Lists expressions such as '(((+ x 1)))
         [(number? op) op]

          ;; For simple arithmatic and relational operators
         [(hash-has-key? env op)
           (apply (hash-ref env op) (map (lambda (arg) (mainStartEval arg Table)) args))]

         
         ;; For conditonal if statement
         [(equal? op 'if)
          (let ([test (mainStartEval (car args))]
               [alternate (mainStartEval (cadr args) Table)]
               [consequent (mainStartEval (caddr args) Table)])
          (if test alternate consequent)
          )]

         ;; For constants for variables we will look up in namespace
         [(equal? op 'quote) (args)]

         [(equal? op 'car) (car (mainStartEval (cadr expr) Table))]

         [(equal? op 'cdr) (cdr (mainStartEval (cadr expr) Table))]

         [(equal? op 'pair?) (pair? (mainStartEval (cadr expr) Table))]

         [(equal? op 'cons)
               (let ([arg1 (mainStartEval (car args) Table)]
                     [arg2 (mainStartEval (cadr args) Table)])
                     (cons arg1 arg2))]
         
         [(equal? op 'lambda)
                         (let ([formalParams (car args)]
                               [actualParams (list (car (cdr (cdr args))))]
                               [functionBody (car (cdr args))])
                               (mainStartEval functionBody (extendingTable Table formalParams actualParams))
                           )]
          ;;(mainStartEval (car (cdr args)) extendingTable Table (car args)))]
               ;; CADR EXPR = ARGS and CADDR EXPR = BODY

        [(equal? op 'let) (mainStartEval (car (cdr args)) (append (car args) Table))]

        [(equal? op 'letrec) (mainStartEval (car (cdr args)) (append (car args) Table))]

        [(list? op) (if (equal? (car op) 'lambda)
                         (let ([formalParams (car (cdr op))]
                               [actualParams args]
                               [functionBody (car (cdr (cdr op)))])
                          (mainStartEval functionBody (extendingTable Table formalParams actualParams)))
                          (let* ([formalParams (car (cdr (car op)))]
                                [actualParams args]
                                [temp (cdr (cdr (car op)))]
                                [functionBody (append (car temp) (cdr op))])
                         (mainStartEval functionBody (extendingTable Table formalParams actualParams))))]

        [else (error "Something wrong")]))]
    [else (tableOperator? expr Table)]))

(define (tableOperator? op Table)
  (cond
    [(null? Table) op] ; If the list is empty, return op
    [(pair? (car Table)) ; If the current element is a pair (like '(x 2))
     (if (equal? (caar Table) op) ; Compare the car of the car with the op
         (car (cdar Table)) ; Return the value (cdr of car)
         (tableOperator? op (cdr Table)))] ; Recurse to the next element
    [else (tableOperator? op (cdr Table))])) ; Else, continue searching

(define (extendingTable Table formalParams actualParams)
  (if (null? formalParams)
      Table
      (extendingTable (append (list (list (car formalParams) (car actualParams))) Table)
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
;;((lambda (x y) (+ x y)) 1 4)
(startEval '(((lambda (x) (lambda (y) (+ x y)))1)2))