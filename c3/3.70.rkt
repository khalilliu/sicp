#lang racket

(require "lib/stream.rkt")

(define (merge-weighted s1 s2 weight)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else 
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (if (<= (weight s1car) (weight s2car))
                (cons-stream s1car
                            (merge-weighted (stream-cdr s1) 
                                            s2
                                            weight))
                (cons-stream s2car
                            (merge-weighted s1 
                                            (stream-cdr s2)
                                            weight))))]))

(define (weighted-pairs s t weight)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (merge-weighted 
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))


(define ordered-sum 
  (weighted-pairs integers
                  integers 
                  (lambda (x) 
                    (apply + x))))

; (show-stream ordered-sum 50)

(define (factored-weight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* 2 i) 
       (* 3 j) 
       (* 5 i j))))

(define (unfactored? x)
  (or (even? x)
           (zero? (remainder x 3))
           (zero? (remainder x 5))))


(define unfactored 
  (stream-filter unfactored? integers))

(define unordered-non-factors
  (weighted-pairs unfactored
                  unfactored
                  factored-weight))

(show-stream unordered-non-factors 10)