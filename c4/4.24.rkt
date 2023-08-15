#lang racket

(require "4.06.rkt"
         (rename-in "4.22.rkt"
                    [interpret analyze+interpret]))

;; Exercise 4.24
;;
(define (test-interpreter run)
   (run '(define (factorial n)
                       (if (< n 2)
                           1
                           (* (factorial (- n 1)) n))))
   (run '(define (fib n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (fib (- n 1)) (fib (- n 2)))))))
  (for ([i (in-range 50)])
     (run '(factorial 50)))
  (for ([i (in-range 20)])
     (run '(fib 20))))

(collect-garbage)
(time (test-interpreter interpret))
;; cpu time: 1373 real time: 1451 gc time: 11
(collect-garbage)
(time (test-interpreter analyze+interpret))
;; cpu time: 253 real time: 267 gc time: 1