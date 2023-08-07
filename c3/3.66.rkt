#lang racket

(require "lib/stream.rkt")

(define (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(stream-ref (pairs integers integers) 197)
; answer: http://community.schemewiki.org/?sicp-ex-3.66
; (1, 100) : 198
; (100,100):2^100 - 1;
