#lang racket

(require "lib/base-eval.rkt")

(provide interpret)

(define boolean-expression-list mcdr)

;; (and (bool-exp) (bool-exp) ...)
;; false if we meet a false value
;; return true if we meet null 
(define (eval-and exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) #t]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) (eval-expression-list (rest-exps exp) env)]
          [else #f]))
  (eval-expression-list (boolean-expression-list exp)))

;; (or (bool-exp) (bool-exp) ...)
;; true if meet a true value
;; return false if we meet null 
(define (eval-or exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) #f]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) #t]
          [else (eval-expression-list (rest-exps exp) env)]))
  (eval-expression-list (boolean-expression-list exp)))


(define (install-boolean-syntax)
  (put-syntax! 'or eval-or)
  (put-syntax! 'and eval-and)
  (void))

(install-boolean-syntax)