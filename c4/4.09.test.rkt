#lang racket/base

(require rackunit
         "4.09.rkt")


(define while-tests
  
  (test-suite
   "Tests for the meta-circular evaluator ex4.09.rkt - while"
   
   (test-case
    "set! mutates outer scope correctly"
    (interpret '(define t 0))
    (test-equal? "set! inside let mutates defined value" 
                 (interpret '(let ((a 4) (b 3))
                               (set! t (* (+ t 4)
                                          (- b t)))
                               t))
                 12)
    (test-equal? "after set! outer scope still correct" 
                 (interpret 't)
                 12))
   
   (test-case
    "test while"
    (interpret '(define xx 5))
    (interpret '(define yy 6))
    (interpret '(while (> xx 0)
                       (set! xx (- xx 1))
                       (set! yy (+ yy 1))))
    (test-equal? "post while condition xx"
                 (interpret 'xx)
                 0)
    (test-equal? "post while condition yy"
                 (interpret 'yy)
                 11)
    (interpret '(while false
                       (set! xx (- xx 1))))
    (test-equal? "post empty while loop"
                 (interpret 'xx)
                 0))   
   ))

(require rackunit/text-ui)
(run-tests while-tests 'normal)
