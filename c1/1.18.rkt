#lang sicp

(define (add x y) (+ x y))
(define (double x) (add x x ))
(define (even x) (= (remainder x 2) 0))
(define (halve x) (/ x 2))

(define (mul x y) (mul-iter x y 0))

(define (mul-iter x y res) (
                   cond ((< x y) (mul-iter y x 0))
                        ((= y 0) res)
                        ((even y) (mul-iter (double x) (halve y) res))
                        (else (mul-iter x (- y 1) (add res x)))
                   ))

(mul 1000000 100)