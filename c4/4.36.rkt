#lang racket

(require "lib/amb.rkt")

(define (a-pythagorean-triple-between low)
  (define (limit n)
    (if (odd? n) ;; 奇数
        (/ (sub1 (square n)) 2) ;; ((n * n) - 1) / 2
        (sub1 (square (/ n 2))))) ;;  (n / 2) * (n / 2) - 1
  (let* ((i (an-integer-starting-from low))
         (j (an-integer-between i (limit i)))
         (k (sqrt (+ (square i) (square j)))))
    (require (integer? k))
    (list i j k)))


(time (a-pythagorean-triple-between 1227))

;;; cpu time: 53 real time: 55 gc time: 37