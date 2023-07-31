#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))

(define a (list 1 2 3))

(subsets a)
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))