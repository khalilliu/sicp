#lang sicp

(define tolerance 0.00001)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (iter (inc i))))))
  (iter 1)
  )

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)


(define (cont-frac2 n d k)
  (define (iter i result)
    (if (zero? i)
        result
        (iter (dec i)
              (/ (n i) (+ (d i) result))))
    )
  (iter k 0.0)
  )

(cont-frac2 (lambda (i) 1.0)
           (lambda (i) 1.0)
           12000000)