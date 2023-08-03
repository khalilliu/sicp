#lang sicp

(define (count-pairs x) 
  (define (collect-pairs x seen) 
    (if (or (not (pair? x)) (memq x seen)) 
        seen 
        (let ((seen-car (collect-pairs (car x) (cons x seen)))) 
          (collect-pairs (cdr x) seen-car)))) 
  (length (collect-pairs x '()))) 


(define x '(foo)) 
(define y (cons x x)) 

(define str1 '(foo bar baz)) 
(count-pairs str1)  ; 3

(define str2 (list y)) 
(count-pairs str2) ; 4 


(define str3 (cons y y)) 

(count-pairs str3) ; 7

