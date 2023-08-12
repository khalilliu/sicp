#lang racket/base

(require rackunit
         "4.04.rkt")

; regressions tests
;(require "ex4.03.test.rkt")

(define boolean-tests
  (test-suite 
   "Tests for the meta-circular evaluator ex4.04.rkt - and and or" 
   
   (test-case "test-or-and"
              (test-false "test-or with empty expression list" (interpret '(or))) 
              (test-false "test-or with single false" (interpret '(or false)))
              (test-true "test-or with single true" (interpret '(or true)))
              (test-true "test-or num-list 1" (interpret '(or 1 2 3)))
              (test-equal? "test-or list #f #f 3" 3 (interpret '(or false false 3)))
              (test-false "test-or #f #f" (interpret '(or false false)))
              (test-equal? "test-and 1 2 3" 3 (interpret '(and 1 2 3)))
              (test-false "test-and #f #f 3" (interpret '(and false false 3)))
              (test-false "test-and #f #f" (interpret '(and false false)))
              (test-true "test-and with empty expression list" (interpret '(and)))
              (test-false "test-and with single false" (interpret '(and false)))
              (test-true "test-and with single true" (interpret '(and true)))
              )))


(require rackunit/text-ui)
(run-tests boolean-tests 'normal)
