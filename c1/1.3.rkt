#lang sicp
(define (min a b) (if (> a b) b a))
(define (max2sum a b c)
  (- (+ a b c)
     (min a (min b c))
     )
)

(max2sum 12 3 3)

