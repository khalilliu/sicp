#lang sicp

(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a)
      (product term (next a) next b))))

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

(define (inc x) (+ x 1))

(define (pi n)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (* 4.0
     (product pi-term 1.0 inc n)))

; (pi 1000000)
; 3.1570301764551645

(define (pi2 n)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (* 4.0
     (product2 pi-term 1.0 inc n)))


(pi2 1000000)
; 3.1570301764551654