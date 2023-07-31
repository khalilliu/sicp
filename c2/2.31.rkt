#lang sicp

(define (square x) (* x x))

(define (tree-map proc tree)
  (map (lambda (x)
         (if (not (pair? x))
             (proc x)
             (tree-map proc x)))
       tree))

(define square-tree
  (lambda (tree) (tree-map square tree)))

;; Test
(define my-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(square-tree my-tree)