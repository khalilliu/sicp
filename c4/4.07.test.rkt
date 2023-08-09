#lang racket/base

(require rackunit
        "4.07.rkt")

; Test suite

(define let*-tests
  (test-suite
    "Tests for the meta-circular evaluator ex4.07.rkt - let*"
    (test-case 
      "test let*"
      (test-equal?
        "let* with 3 variabes"
        39
        (interpret 
          '(let* ((x 3)
                  (y (+ x 2))
                  (z (+ x y 5)))
             (* x z))))
             
    (test-equal? 
      "let* with a sequence of expression in the body"
      -8
      (interpret '(let* ((x 3)
                      (y (+ x 2))
                      (z (+ x y 5)))
                  (* x z)
                  (+ x y z)
                  (* (- y z)))))
)))

(require rackunit/text-ui)
(run-tests let*-tests 'normal)