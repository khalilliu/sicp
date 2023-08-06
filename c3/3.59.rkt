#lang racket

(require "lib/stream.rkt")

(define (integrate-seris coeffs)
  (stream-map / coeffs integers))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (scale-stream 
                  (integrate-series cosine-series) -1)))
