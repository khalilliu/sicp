#lang racket 

(require (except-in "lib/base-eval.rkt"
                    definition?)
         "4.04.rkt"  ;; boolean expression
         "4.05.rkt"  ;; cond with =>
         "4.06.rkt"  ;; let
         "4.07.rkt"  ;; let*
         "4.08.rkt"  ;; named-let
         "4.09.rkt"  ;; while
         ) 


(provide interpret)

;; Exercise 4.10
;;
;; this version of the evaluator includes these expression types:
;;  self-evaluating 
;;  variable
;;  quote 
;;  define =>> fun
;;  set! 
;;  if
;;  lambda 
;;  begin 
;;  application? 
;;  and =>> &&
;;  or =>> ||
;;  cond 
;;  special cond <test> => <recipient>
;;  let
;;  let*
;;  named let
;;  while


; Boolean expressions
;
;; These could have been exported from ex4.04.rkt
;; but instead are repeated here
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

(define (definition? exp)
  (tagged-list? exp 'fun))

(define (install-boolean-syntax)
  (put-syntax! '||  eval-or)
  (put-syntax! '&&  eval-and)
  (put-syntax! 'fun eval-definition)
  (void))

(install-boolean-syntax)

