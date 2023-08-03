#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (begin (display x) (newline)
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1))))

(define x '(foo)) 
(define y (cons x x)) 

(define str1 '(foo bar baz)) 
(count-pairs str1)  ; 3

(define str2 (list y)) 
(count-pairs str2) ; 4 


(define str3 (cons y y)) 

(count-pairs str3) ; 7

