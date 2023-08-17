#lang racket

(require "lib/ramb-eval-require.rkt")

;; Exercise 4.54

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items)))))

(interpret '(define (smallest-divisor n)
              (find-divisor n 2)))

(interpret '(define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (+ test-divisor 1))))))

(interpret '(define (divides? a b)
              (= (remainder b a) 0)))

(interpret '(define (square x)
              (* x x)))

(interpret '(define (prime? n)
              (= n (smallest-divisor n))))

(interpret '(define (prime-sum-pair list1 list2)
              (let ((a (an-element-of list1))
                    (b (an-element-of list2)))
                (require (prime? (+ a b)))
                (list a b))))

(interpret '(let ((pairs '()))
              (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                         (permanent-set! pairs (cons p pairs))
                         (amb))
                       pairs)))

;;; '((8 35) (3 110) (3 20))