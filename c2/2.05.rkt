#lang racket

(define (cons-e a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car-e z)
  (iter-div z 2 0))

(define (cdr-e z)
  (iter-div z 3 0))

(define (inc x) (+ x 1))

(define (iter-div n d i)
  (if (= 0 (remainder n d))
      (iter-div (/ n d) d (inc i))
      i))

; test case 
(define z-e (cons-e 2 5)) 
(car-e z-e) 
(cdr-e z-e) 
