#lang sicp

(define (f g) (g 2))

(define (square a) (* a a))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)