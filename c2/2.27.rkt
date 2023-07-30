#lang sicp

(define x '((1 (2 (12 13))) (3 4)))

(define (deep-reverse items)
  (cond [(null? items) nil]
        [(pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items))))]
        [else
         (append (deep-reverse (cdr items))
                 (list (car items)))]))

;; test
x
(deep-reverse x)