#lang racket

(define (make-interval a  b) (cons a b))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

(define (print-interval i) 
  (newline) 
  (display "[") 
  (display (lower-bound i)) 
  (display ",") 
  (display (upper-bound i)) 
  (display "]"))

;; Usage 
(define i (make-interval 2 7)) 
(define j (make-interval 8 3))

(print-interval i)
(print-interval j) 
(print-interval (sub-interval i j)) 
(print-interval (sub-interval j i)) 