#lang racket

(define env (hash '+ + '- - '/ / '* * '= = '<= <= '< < '>= >= '> >))

(define (startEval expr Table)
  (cond
    [(number? expr) expr]
    [(pair? expr)
     (let ([op (car expr)]
           [args (cdr expr)])
       (cond
         ;; For evaluating nested Lists expressions such as '(((+ x 1)))
         [(pair? op) (startEval op Table)]

         ;; For simple arithmatic and relational operators
         [(hash-has-key? env op)
           (apply (hash-ref env op) (map startEval args Table))]

         ;; For conditonal if statement
         [(equal? op 'if)
          (let ([test (startEval (car args))]
               [alternate (startEval (cadr args) Table)]
               [consequent (startEval (caddr args) Table)])
          (if test alternate consequent)
          )]

         ;; For constants for variables we will look up in namespace
         [(equal? op 'quote) (cadr expr)]

         [(equal? op 'car) (car (startEval (cadr expr) Table))]

         [(equal? op 'cdr) (cdr (startEval (cadr expr) Table))]

         [(equal? op 'pair?) (pair? (startEval (cadr expr) Table))]

         [(equal? op 'cons)
               (let ([arg1 (startEval (car args) Table)]
                     [arg2 (startEval (cadr args) Table)])
                     (cons arg1 arg2))]
         
         [(equal? op 'lambda)
               (list 'closure (cadr expr) (caddr expr))]

         [else (error "Some thing wrong")]))]

    [else (error "Some thing wrong")]))

(define ( processFunction params body) #t)



(startEval '(((+ 2 1))))