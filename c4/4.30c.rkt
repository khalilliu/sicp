#lang racket 

;; Exercise 4.30c

(require "lib/lazy-eval-cy.rkt")

(interpret '(define (for-each proc items)
              (if (null? items)
                  'done
                  (begin (proc (car items))
                         (for-each proc (cdr items))))))

(interpret '(for-each (lambda (x) (display x) (newline))
                      (list 57 321 88)))

(interpret '(define (p1 x)
              (set! x (cons x '(2)))
              x))

(interpret '(define (p2 x)
              (define (p e)
                e
                x)
              (p (set! x (cons x '(2))))))


(interpret '(p1 1))
(interpret '(p2 1))

;; NB identical to the original evaluator
;;
;;57
;;321
;;88
;;'done
;;'(1 2)
;;'(1 2)