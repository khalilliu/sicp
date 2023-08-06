#lang racket

(require "lib/stream.rkt")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) 
                                          (stream-car s2))
                            (mul-series s1 
                                        (stream-cdr s2)))))


(define (invert-unit-series s)
  (cons-stream 
   1
   (scale-stream (mul-series (stream-cdr s)
                             (invert-unit-series s))
                 -1)))


(provide invert-unit-series 
         mul-series)