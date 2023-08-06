#lang racket

(require "lib/constraint.rkt")

(define (averager a b average)
  (let ((summed (make-connector))
        (two (make-connector)))
    (constant 2 two)
    (adder a b summed)
    (multiplier two average summed)))