#lang racket

(require "lib/stream.rkt")

;; Exercise 3.80
;;
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (RLC R L C dt)
  (lambda (il0 vc0)
    (let* ((il (integral
               (delay
                 (add-streams (scale-stream il (- (/ R L)))
                              (scale-stream vc (/ 1.0 L))))
               il0
               dt))
          (vc (integral
               (delay (scale-stream il (- (/ 1.0 C))))
               vc0
               dt)))
      (cons vc il))))


(define rlc ((RLC 1 1 0.2 0.1) 0 10))
(define vc (car rlc))
(define il (cdr rlc))


(show-stream vc 5)
; 10
; 10
; 9.5
; 8.55
; 7.220000000000001
;
(show-stream il 5)
; 0
; 1.0
; 1.9
; 2.66
; 3.249


