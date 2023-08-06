#lang racket

(require "lib/stream.rkt")

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(define a (stream-ref x 5))
;; 0 1 2 3 4 5
(define b (stream-ref x 7))
;; 6 7

a 
;; => 5
b
;; => 7