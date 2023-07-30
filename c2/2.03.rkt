#lang racket

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (make-rectangle p1 p2) (cons p1 p2))

(define (start-point rect) (car rect))

(define (end-point rect) (cdr rect))

(define (perimeter-rectangle rect)
  (define (get-distance a b)
    (abs (- a b)))
  (let ((p1 (start-point rect))
        (p2 (end-point rect)))
    (+ (* (get-distance (x-point p1) (x-point p2)) 2.0)
       (* (get-distance (y-point p1) (y-point p2)) 2.0))))

(define (area-rectangle rect)
  (define (get-distance a b)
    (abs (- a b)))
  (let ((p1 (start-point rect))
        (p2 (end-point rect)))
    (* (get-distance (x-point p1) (x-point p2))
       (get-distance (y-point p1) (y-point p2)))))


(define p1 (make-point 2 2))
(define p2 (make-point 4 4))
(define rect (make-rectangle p1 p2))

(perimeter-rectangle rect)
(area-rectangle rect)
