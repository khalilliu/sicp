#lang racket

(require "lib/stream.rkt")

(define (integral intergrand initial-value dt)
  (define int
    (cons-stream initial-value 
                  (add-streams (scale-stream intergrand dt)
                                int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                            v0
                            dt))))

(define RC1 (RC 5 1 0.5))

(show-stream (RC1 integers 0.2) 5)
; 5.2
; 10.7
; 16.7
; 23.2
; 30.2
