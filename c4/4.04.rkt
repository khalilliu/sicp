#lang racket

(require "lib/base-eval.rkt")
(provide interpret)

; Boolean expressions
(define boolean-expression-list mcdr)

(define (eval-and exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) true]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) (eval-expression-list (rest-exps exp))]
          [else false]))
  (eval-expression-list (boolean-expression-list exp)))

(define (eval-or exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) false]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) true]
          [else (eval-expression-list (rest-exps exp))]))
  (eval-expression-list (boolean-expression-list exp)))


(define (install-boolean-syntax)
  (put-syntax! 'or eval-or)
  (put-syntax! 'and eval-and)
  (void))

(install-boolean-syntax)

(interpret '(or false))