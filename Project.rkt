#lang racket

(define env (hash '+ + '- - '/ / '* * '= = '<= <= '< < '>= >= '> >))

(define (mainStartEval expr Table)
  (cond
    [(number? expr) expr]
    [(pair? expr)
     (letrec ([op (car expr)]
              [args (cdr expr)]
              [fromTable (tableOperator? op Table)])
       (cond
         
         [(pair? op) (mainStartEval op Table)]
         
         ;; For evaluating nested Lists expressions such as '(((+ x 1)))
         [(not (equal? #f fromTable)) fromTable]

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
               (list 'closure (cadr expr) (caddr expr))]

        [(equal? op 'let) (mainStartEval (cdr args) (append (car args) Table))]

        g[(equal? op 'letrec) (mainStartEval (cdr args) (append (car args) Table))]

        [else (error "Something wrong")]))]
        [(let ([fromTable (tableOperator? expr Table)])
      (not (equal? fromTable #f)) (car fromTable))]
    [else (error "Some thing wrong")]))



(define (tableOperator? op Table)
  (cond
    [(null? Table) #f] ; If the list is empty, return #f
    [(pair? (car Table)) ; If the current element is a pair (like '(x 2))
     (if (equal? (caar Table) op) ; Compare the car of the car with the op
         (cdar Table) ; Return the value (cdr of car)
         (tableOperator? op (cdr Table)))] ; Recurse to the next element
    [else (tableOperator? op (cdr Table))])) ; Else, continue searching


(define (startEval sourceCode) (mainStartEval sourceCode '()))

(startEval '(let ([x 2] [y 2]) (+ x y)))