#lang sicp

(define (square x) (* x x))

(define (square-tree-1 tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree)))]))

(define (square-tree-2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-2 x)
             (square x)))
       tree))

(define my-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(square-tree-1 my-tree)

(square-tree-2 my-tree)