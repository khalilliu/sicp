#lang racket

(require "lib/wires.rkt"
          "3.29.rkt"
          "3.30.rkt")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; (probe 'sum sum)

; (probe 'carry carry)

(probe 'input-1 input-1)

; (half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)


;  Without executing the procedure immediately the system would never actually start – 
;  nothing would be added to the agenda, no actions would occur and no signals would propagate.
