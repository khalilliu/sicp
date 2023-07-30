#lang racket

(define nil '())

(define (same-parity x . rest)
  (define (iter target items result)
    (cond [(null? items) result]
          [(= target
              (remainder (car items) 2))
           (iter target (cdr items) (append result (cons (car items) nil)))]
          [else (iter target (cdr items) result)]
          ))
  (iter (remainder x  2) rest (cons x nil)))

;; test
(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)