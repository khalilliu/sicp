#lang racket

(require "lib/stream.rkt")

(define (pairs s t)
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(show-stream (pairs integers integers) 100)


;; no. 
;; The program will infinitely loop. because their is no delay 
;; in (pairs (stream-cdr s) (stream-cdr t))
;; it will be called recursively.
