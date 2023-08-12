#lang racket/base

(require rackunit
         "4.06.rkt")

; regressions tests
;(require "ex4.03.test.rkt"
;         "ex4.04.test.rkt"
;         "ex4.05.test.rkt")
(define let-tests
  (test-suite 
   "Tests for the meta-circular evaluator ex4.06.rkt - let"
    (test-case 
      "test let"
      (test-equal? 
      "simple let"
      6
      (interpret '(let ((a 1) (b 2) (c 3))
                    (+ a b c))))
      
      (interpret '(define (square x) (* x x)))
      (interpret '(define (square-1 x) (+ (square x)
                                        (square (- x 1)))))
      (test-equal?
      "complex let with procedure calls"
      453220
      (interpret '(let ((a (square-1 7))
                       (b (+ (* 17 (square-1 13)) 11)))
                   (* a b)))))
))



(require rackunit/text-ui)
(run-tests let-tests 'normal)
