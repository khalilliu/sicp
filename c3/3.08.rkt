#lang racket

(define f
  (let ((has-called-with null))
    (lambda (x)
      (if (null? has-called-with)
        (begin (set! has-called-with x) (* x has-called-with))
        (* x has-called-with)))))

;(+ (f 0) (f 1))
; => 0
; (+ (f 1) (f 0))
; => 1