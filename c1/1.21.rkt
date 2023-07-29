#lang sicp

(define (smallest-divisor n) (find-divisor n 2))
(define (square x) (* x x))

(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))
        )
  )

(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)