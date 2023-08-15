#lang racket/base
(require rackunit
         "4.26.rkt")

; Test suite


(define unless-tests
  (test-suite
   "Tests for the meta-circular analysing evaluator ex4.26.rkt" 
   (test-case 
    "Unless as a special form"
    (interpret '(define (unless-factorial n)
                  (unless (= n 1)
                    (* n (unless-factorial (- n 1)))
                    1)))
    
    (test-equal? "Unless factorial 5"
                 (interpret '(unless-factorial 5))
                 120))
   ))

(require rackunit/text-ui)
(run-tests unless-tests 'normal)
