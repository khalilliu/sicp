#lang racket 

;; Exercise 4.29
(require "lib/lazy-eval.rkt")

(printf "~nDefining the fib procedure~n")
(collect-garbage)
(time
 (interpret '(define (fib n)
               (cond ((= n 0) 0)
                     ((= n 1) 1)
                     (else (+ (fib (- n 1))
                              (fib (- n 2))))))))
(printf "~nCalling fib 3 times~n")
(time (for ([i (in-range 3)])
        (interpret '(fib 20))))


;;; Defining the fib procedure
;;; cpu time: 0 real time: 0 gc time: 0

;;; Calling fib 3 times
;;; cpu time: 250 real time: 262 gc time: 1