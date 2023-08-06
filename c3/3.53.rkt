#lang racket

(require "lib/stream.rkt")

; (define primes
;   (cons-stream 2
;     (stream-filter prime? (integers-starting-from 3))))

; (define (prime? n)
;   (define (iter ps)
;     (cond [(> (square (stream-car ps)) n) true]
;           [(divisible? n (stream-car ps)) false]
;           [else (iter (stream-cdr ps))]))
;   (iter primes))

; (define (divisible? x y)
;   (= (remainder x y) 0))

; (define (square x)
;   (* x x))

; (show-stream primes 10)


(define s (cons-stream 1 (add-streams s s)))

(show-stream s 10)
; 1
; 2
; 4
; 8
; 16
; 32
; 64
; 128
; 256
; 512