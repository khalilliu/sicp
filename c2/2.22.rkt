#lang sicp

(define (square x) (* x x))

(define (square-list input)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result
                    (cons (square (car items)) nil)))))
  (iter input nil)
  )

(square-list (list 1 2 3 4))