#lang racket

(require "lib/stream.rkt")

(define (integrate-seris coeffs)
  (stream-map / coeffs integers))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream 0 (scale-stream 
                  (integrate-series cosine-series) -1)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))

                (add-streams (scale-stream (stream-cdr s1)
                                           (stream-car s2))
                              (mul-series s1 
                                          (stream-cdr s2)))))

(define cos^2+sin^2
  (add-streams (mul-series cosine-series
                           cosine-series)
               (mul-series sine-series
                           sine-series)))

