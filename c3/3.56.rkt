#lang racket

(require "lib/stream.rkt")

(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else (let ([s1car (stream-car s1)]
                    [s2car (stream-car s2)])
                (cond [(< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2))]
                      [(> s1car s2car)
                       (cons-stream s2car (merge s1 (stream-cdr s2)))]
                      [else (cons-stream s1car
                                         (merge (stream-cdr s1)
                                                (stream-cdr s2)))]))]))



(define hammings 
  (cons-stream 
   1 
   (merge (scale-stream hammings 2)
          (merge (scale-stream hammings 3)
                 (scale-stream hammings 5)))))



(show-stream hammings 10)