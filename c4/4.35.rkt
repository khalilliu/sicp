#lang racket

(require "lib/amb.rkt")

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high))
         (k (an-integer-between j high)))
    (require (= (+ (square i) (square j))
                (square k)))
    (list i j k)))


(time (a-pythagorean-triple-between 1227 1700))

;;; (1227 1636 2045)