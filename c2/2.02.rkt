#lang racket

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2.0)
                (/ (+ (y-point p1) (y-point p2)) 2.0))))

(define p1 (make-point -1 21))
(define p2 (make-point 4 4))
(define s1 (make-segment p1 p2))
(print-point (midpoint-segment s1))

