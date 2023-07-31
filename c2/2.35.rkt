#lang sicp

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (enumerate-tree t)
  (cond [(null? t) nil]
        [(not (pair? t)) (list t)]
        [else (append (enumerate-tree (car t))
                      (enumerate-tree (cdr t)))]))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))


;; Usage 
(define tree (list 1 2 3 (list 4 5 (list 6 7)))) 
(count-leaves tree)
;; => 7 