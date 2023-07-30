#lang racket

(define nil '())

(define (reverse-recur items)
  (if (null? (cdr items))
      items
      (append (reverse-recur (cdr items)) (cons (car items) nil))))

(define (reverse-iter items)
  (define (iter i res)
    (if (null? i)
        res
        (iter (cdr i) (cons (car i) res))))
  (iter items nil))

(reverse-recur (list 1 4 9 16 25))

(reverse-iter (list 1 4 9 16 25))