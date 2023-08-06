#lang racket

(require
  "lib/wires.rkt")

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry sum)


(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

; (set-signal! input-2 1)

; (propagate)