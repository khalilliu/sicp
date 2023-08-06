#lang racket

(require "lib/stream.rkt")

(define sum 0)

(define (even? x)
  (= (remainder x 2) 0))

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)
;; 136
(display-stream seq)
;; 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210

(display-stream z)
;; 10 15 45 55 105 120 190 210