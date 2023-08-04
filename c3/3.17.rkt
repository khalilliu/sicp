#lang sicp

(define (count-pairs x)
  (define (iter cur visited)
    (if (or (not (pair? cur)) (memq cur visited))
        visited
        (let ((visited-cars (iter (car cur) (cons cur visited))))
            (iter (cdr cur) visited-cars) )))
  (length (iter x '())))


(define x '(foo)) 
(define y (cons x x)) 

(define str1 '(foo bar baz)) 
(count-pairs str1)  ; 3

(define str2 (list y)) 
(count-pairs str2) ; 3


(define str3 (cons y y)) 

(count-pairs str3) ; 3

