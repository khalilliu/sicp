#lang sicp

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 


(define (horner-eval x sequence)
  (accumulate (lambda (cur acc)
                (+ cur (* x acc)))
              0
              sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
;; 79