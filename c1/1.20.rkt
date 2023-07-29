#lang sicp

;;; calc gcd of input a b
(define (gcd a b) (if (= b 0) a (gcd  b (remainder a b))))

(gcd 321328 1204)




