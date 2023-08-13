#lang racket/base

(require rackunit
         "4.16c.rkt")

;; regression tests
;(require rackunit
;         "ex4.03.test.rkt"
;         "ex4.04.test.rkt"
;         "ex4.05.test.rkt"
;         "ex4.06.test.rkt"
;         "ex4.07.test.rkt"
;         "ex4.08.test.rkt"
;         "ex4.09.test.rkt"
;         "ex4.13.test.rkt")


; Test suite

(define internal-definition-tests
  (test-suite
   "Tests for the meta-circular evaluator ex4.16c.rkt - internal definitions via transformation"
  
   (test-case 
    "internal definitions with recursion"
    (interpret '(define (f x)
                  (define (not-even? n)
                    (if (= n 0)
                        false
                        (not-odd? (- n 1))))
                  (define (not-odd? n)
                    (if (= n 0)
                        true
                        (not-even? (- n 1))))
                  (if (> x 2)
                      (not-even? x)
                      true)))
    (test-false "simple recursive internal definition f(8) => false"
                (interpret '(f 8)))
    (test-true "simple recursive internal definition f(2) => true"
               (interpret '(f 2)))
    (test-true "simple recursive internal definition f(3) => true"
               (interpret '(f 3)))
    
    (test-equal? "stateful recursive internal definition"
                 (interpret '((lambda (x y)
                                (define (u a) (cond ((= a 0) 0)
                                                    (else (v (- a 1)))))
                                (define (v a) (cond ((= a 0) 0)
                                                    (else (u (- a 1)))))
                                (u (+ x y)))
                              3 7))
                 0))
   (test-case 
    "self-eval"
    (test-equal? "self-eval integer" 5 (interpret 5))
    (test-equal? "self-eval bignum" 666666666222 (interpret 666666666222))
    (test-equal? "self-eval rational" (/ 3 7) (interpret (/ 3 7)))
    (test-equal? "self-eval string" "hey" (interpret "hey")))
   ))

(require rackunit/text-ui)
(run-tests internal-definition-tests 'normal)