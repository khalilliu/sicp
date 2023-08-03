#lang sicp

(define (make-accumulator initial-value)
  (lambda (next-value)
    (begin (set! initial-value 
                (+ initial-value next-value))
            initial-value)))


    
(define acc (make-accumulator 5))

(acc 10)
;; => 15
(acc 10)
;; => 25