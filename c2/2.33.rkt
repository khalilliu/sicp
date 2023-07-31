#lang sicp

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) nil sequence))

;; Test: 

(my-map square (list)) 
(my-map square (list 1 2 3 4))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Test: 
  
(append (list 1 2 3) (list 4 5 6))  ;; checking order. 
(my-append (list 1 2 3) (list 4 5 6)) 
  

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; Test: 
(length (list 1 2 3 (list 4 5))) 
(my-length (list 1 2 3 (list 4 5))) 
  

