#lang racket

(require "lib/wires.rkt")

; (define (or-gate a1 a2 output)
;   (let ((c (make-wire)) 
;         (d (make-wire)) 
;         (e (make-wire)) 
;         (f (make-wire) 
;         (g (make-wire))))
;     (and-gate a1 a1 d)
;     (and-gate a2 a2 e)
;     (inverter d f)
;     (inverter e g)
;     (and-gate f g c)
;     (inverter c output)
;     'ok))


(provide or-gate)  

(define (or-gate a b s)
  (with-wires (na nb t)
    (inverter a na)
    (inverter b nb)
    (and-gate na nb t)
    (inverter t s))
)


