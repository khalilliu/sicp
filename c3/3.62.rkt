#lang racket

(require "lib/stream.rkt"
         "3.61.rkt")

;; copy code []
(define (div-series num denom)
  (let ((denom-const (stream-car denom)))
    (if (zero? denom-const)
        (error ("DIV-SERIES -- denominator constant term must be non-zero" ))
        (mul-series num 
                    (scale-stream ; restore the scaling factor 
                     (invert-unit-series  ; requires a stream that has a unit constant term
                      (scale-stream denom (/ 1 denom-const)))
                     denom-const)))))


(define tan-series (div-series sine-series 
                               cosine-series))
