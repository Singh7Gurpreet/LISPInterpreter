#lang racket

(define env (hash '+ + '- - '/ / '* * '= = '<= <= '< < '>= >= '> >))

(define (mainStartEval expr Table)
  (cond
    [(number? expr) expr]
    [(pair? expr)
     (letrec ([op (tableOperator? (car expr) Table)]
              [args (cdr expr)])
       (cond
         
         [(pair? op) (mainStartEval op Table)]
         
         ;; For evaluating nested Lists expressions such as '(((+ x 1)))
         [(number? op) op]

          ;; For simple arithmatic and relational operators
         [(equal? op '+) (+ (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '-) (- (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '*) (* (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '/) (/ (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '=) (= (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '<=) (<= (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '<) (< (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '>=) (>= (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         [(equal? op '>) (> (mainStartEval (car args) Table) (mainStartEval (cadr args) Table))]
         
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
               (list 'closure (cadr expr) (caddr expr))]

        [(equal? op 'let) (mainStartEval (cdr args) (append (car args) Table))]

        [(equal? op 'letrec) (mainStartEval (cdr args) (append (car args) Table))]

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


(define (startEval sourceCode) (mainStartEval sourceCode '()))

(tableOperator? 'y '((x 2) (y 4)))
(startEval '(letrec ([y 1] [x 2]) (+ x y)))
(startEval '(letrec ([* +]) (* 5 5)))