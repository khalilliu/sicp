#lang racket/base

(require rackunit
         "4.08.rkt")

; Test suite

(define named-let-tests
  
  (test-suite
   "Tests for the meta-circular evaluator ex4.08.rkt - named-let"


    (test-case 
      "named let"
      (interpret '(define (fib n)
                      (let fib-iter ((a 1) (b 0) (count n)) 
                        (if (= count 0) 
                            b
                            (fib-iter (+ a b) a (- count 1))))))

      (test-equal? "named let 1"
                 13 
                 (interpret '(fib 7)))
      (test-equal? "named let 2"
                 21 
                 (interpret '(fib 8)))
      (test-equal? 
        "recursive name-let to reverse a list"
        '(k j i h g f e d c b a)
        (interpret '(let rev-list ((xs '(a b c d e f g h i j k))
                                        (rev '()))
                          (cond ((null? xs) rev)
                                (else (rev-list (cdr xs) 
                                                (cons (car xs) rev))))))
))))

(require rackunit/text-ui)
(run-tests named-let-tests 'normal)
