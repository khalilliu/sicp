#lang racket

(require "lib/stream.rkt")

(define (list->stream xs)
  (foldr (lambda (x ys)
            (cons-stream x ys))
          the-empty-stream
          xs))

(define sense-data 
  (list->stream 
   (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 0 0 0 0 0 -1 0 0 0 0 1 0 0)))

(define (sign-change-detector current previous)
  (cond [(and (>= current 0)
              (< previous 0)) 1]
        [(and (< current 0)
              (>= previous 0)) -1]
        [else 0]))

(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2.0)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                     (stream-car input-stream)
                                     avpt))
  ))

(define smoothed-zero-crossings 
  (make-zero-crossings sense-data 0 0)) 

(show-stream smoothed-zero-crossings 10)


