#lang sicp

(define (fold-right op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-using-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (reverse-using-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))


;Test
(reverse-using-left '(1 2 3 4 5))  
(reverse-using-right '(1 2 3 4 5))  
;Value: (5 4 3 2 1) 
  

