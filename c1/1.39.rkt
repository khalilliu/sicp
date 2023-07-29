#lang sicp

(define tolerance 0.00001)

(define (identify x) x)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (tan-cf x k)
  (define (n i) (if (eq? i 1)
                   (identify x)
                    (square x)))
   (define (d i) (dec (* i 2.0))) 
   (define (iter i)
       (if (> i k)
        0
        (/ (n i x) (- (d i) (iter (inc i))))))
     
    (iter 1))


(define (tan-cf2 x k)
  (define (n i) (if (eq? i 1)
                   (identify x)
                   (square x)))
  (define (d i) (dec (* i 2.0)))
  (define (iter i result)
    (if (zero? i)
        result
        (iter (dec i)
              (/ (n i) (- (d i) result))))
    )
  (iter k 0.0)
  )

;(tan-cf 0.785398163397448 1200000) // out of memory

(tan-cf2 0.785398163397448 1200000)
;0.9999999999999993


