#lang racket

(require "lib/stream.rkt")

(define (solve f y0 dt)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (delayed-integral delayed-integrand initial-value dt)
  (cons-stream initial-value
              (if (stream-null? delayed-integrand)
                  the-empty-stream
                  (let ((integrand (force delayed-integrand)))
                       (delayed-integral (delay (stream-cdr integrand))
                                         (+ (* dt (stream-car integrand))
                                            initial-value)
                                          dt)))))

;; (stream-ref (solve (lambda (y) y) 1 0.00001) 100000)
;; 2.7182682371744953

