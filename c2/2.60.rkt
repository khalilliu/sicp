#lang sicp

(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set))))) 
  
(define (adjoin-set x set) 
  (cons x set)) 
  
(define (union-set set1 set2) 
  (append set1 set2)) 

(define (remove-set-element x set)
  (define (iter acc rest)
    (cond [(null? rest) acc]
          [(equal? x (car rest)) (append acc (cdr rest))]
          [else (iter (adjoin-set (car rest) acc) (cdr rest))]))
  (iter '() set))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) nil]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) (remove-set-element (car set1) set2)))]
        [else (intersection-set (cdr set1) set2)]))


(intersection-set '(1 2 3 4 1) '(2 1 2 3 4 1 1))