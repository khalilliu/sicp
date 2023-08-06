#lang racket

(require "lib/stream.rkt")


(define (expand num den radix)
  (cons-stream 
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))


(show-stream (expand 1 7 10) 12) ;; 1 / 7
;; 142857
(show-stream (expand 3 8 10) 10) ;; 3 / 8
;; 3 7 5 0 0 ...