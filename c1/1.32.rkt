#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value)
  )

(define (inc x) (+ x 1))

(define (add a b) (+ a b))

(define (product a b) (* a b))

(define (identify a) a)

(accumulate add 0 identify 1 inc 100)
; 5050
(accumulate2 add 0 identify 1 inc 100)

(accumulate product 1 identify 1 inc 5)
; 120
(accumulate2 product 1 identify 1 inc 5)