#lang sicp

(define x '((1 (2 (3 4))) (5 (6 7))))

(define (fringe items)
  (cond [(null? items) nil]
        [(pair? items)
         (append (fringe (car items))
                 (fringe (cdr items)))]
        [else (list items)]))

x
(fringe x)