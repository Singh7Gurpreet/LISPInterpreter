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
         [(not(equal? #f fromTable)) fromTable]

         [(equal? op 'let) (mainStartEval (cdr args) (append (list (car args)) Table))]

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

        [(equal? op 'letrec) (append (mainStartEval args Table) Table)]

        [else (error "Something wrong")]))]

    [else (error "Some thing wrong")]))



(define (tableOperator? op Table)
  (if (null? Table)
      #f  ; Return #f if the table is empty (operator not found)
      (if (equal? op (car (car Table))) (cdr (car Table)) (tableOperator? op (cdr Table)))))


(define (startEval sourceCode) (mainStartEval sourceCode '()))

(startEval '((+ 2 1)))
(startEval '(let [x 2] (+ x 1)))#lang racket

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
         [(not(equal? #f fromTable)) fromTable]

         [(equal? op 'let) (mainStartEval (cdr args) (append (list (car args)) Table))]

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

        [(equal? op 'letrec) (append (mainStartEval args Table) Table)]

        [else (error "Something wrong")]))]

    [else (error "Some thing wrong")]))



(define (tableOperator? op Table)
  (if (null? Table)
      #f  ; Return #f if the table is empty (operator not found)
      (if (equal? op (car (car Table))) (cdr (car Table)) (tableOperator? op (cdr Table)))))


(define (startEval sourceCode) (mainStartEval sourceCode '()))

(startEval '((+ 2 1)))
(startEval '(let [x 2] (+ x 1)))