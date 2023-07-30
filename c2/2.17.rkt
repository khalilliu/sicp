#lang racket


;; recursion
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; iteration
(define (last-pair-iter items)
  (define (iter i res)
    (if (null? i)
        res
        (iter (cdr i) i)))
  (iter items items))

(last-pair (list 23 72 149 34))

(last-pair-iter (list 23 72 149 34))