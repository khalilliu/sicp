#lang racket

(require "lib/stream.rkt")

(define (partial-sum s)
  (cons-stream (stream-car s)
                (add-streams (stream-cdr s) 
                             (partial-sum s))))

(show-stream (partial-sum integers) 6)
;; => 1 3 6 10 15 21
