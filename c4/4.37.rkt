#lang racket
(require "lib/amb.rkt")

;; Exercise 4.37

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (square i) (square j))))
        (require (>= (square high) ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(time (a-pythagorean-triple-between 1227 10000))

;;; cpu time: 50 real time: 51 gc time: 35
;;; (1227 1636 2045)