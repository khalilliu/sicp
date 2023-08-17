#lang racket

(require "lib/ramb-eval.rkt")

;; Exercise 4.52

(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items)))))


(interpret '(if-fail (let ((x (an-element-of '(1 3 5))))
                        (require (even? x))
                        x)
                      'all-odd))

;;; 'all-odd

(interpret '(if-fail (let ((x (an-element-of '(1 3 5 8))))
                       (require (even? x))
                       x)
                     'all-odd))

;;; 8