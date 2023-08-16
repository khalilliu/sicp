#lang racket 

(require "lib/lazy-list-eval.rkt")

;; Exercise 4.34

(interpret '(car '(a b c)))
(interpret '(car (cdr (cdr '(a b c)))))

(interpret '(define (scale-list items factor)
              (map (lambda (x) (* x factor))
                   items)))
(interpret '(define (add-lists list1 list2)
              (cond ((null? list1) list2)
                    ((null? list2) list1)
                    (else (cons (+ (car list1) (car list2))
                                (add-lists (cdr list1) (cdr list2)))))))

(interpret '(define ones (cons 1 ones)))
(interpret '(define integers (cons 1 (add-lists ones integers))))
(interpret '(display-lazy-list ones))
(interpret '(display-lazy-list integers))