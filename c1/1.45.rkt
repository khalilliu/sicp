#lang sicp

(define (close-enough? v1 v2)
  (define tolerance 1.e-6)
  (< (/ (abs (- v1 v2)) v2)  tolerance))

(define (iter-improve good-enough? improve)
  (lambda (x)
    (let ((xx (improve x)))
      (if (good-enough? x xx)
          x
          ((iter-improve good-enough? improve) (improve x))))))


(define (sqrt x)
  (define (average a b) (/ (+ a b) 2.0))
  ((iter-improve
    close-enough?
    (lambda (y)
      (average y (/ x y))))
   1.0))

(sqrt 2)