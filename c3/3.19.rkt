#lang sicp

;; like find a cycle in linked-list
;; use two pointer to iterate the list
;; quick-pointer scan 2 element one step, slow-pointer 1 elemenet
(define (cycle? lst) 
  (define (safe-cdr x)
    (if (pair? x) (cdr x) nil)) 
  (define (iter a b) 
    (cond [(not (pair? a)) #f]
          [(not (pair? b)) #f]
          [(eq? a b) #t]
          [(eq? a (safe-cdr b)) #t]
          [else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))])) 
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst)))) 

(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cddr l1) l1)
(cycle? l1)
;; #t
(cycle? l2)
;; #f