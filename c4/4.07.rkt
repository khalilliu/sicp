#lang racket 

(require "lib/base-eval.rkt"
         "ex4.06.rkt"  ;; let
         ) 

(provide interpret)

;; Exercise 4.7
;;
;; this version of the evaluator includes these expression types:
;;  self-evaluating 
;;  variable
;;  quote 
;;  define 
;;  set! 
;;  if
;;  lambda 
;;  begin 
;;  application? 
;;  and
;;  or
;;  cond 
;;  special cond <test> => <recipient>
;;  let
;;  let*

(define (let-initials exp) 
  (mmap mcadr (mcadr exp)))
  
(define (let-parameters exp)
  (mmap mcar (mcadr exp)))
  
(define (let-body exp)
  (mcddr exp))

; a let is syntactic sugar for
;   ((lambda (params) (body)) values)
(define (let->combination exp)
  (mcons (make-lambda (let-parameters exp) (let-body exp))
        (let-initials exp)))

(define (eval-let exp env)
  (eval (let->combination exp)  env))

;  a let* is syntactic sugar for nested lets
(define (let*->nested-lets exp)
  (define (make-let params)
    (cond [(last-exp? params) (mappend (mlist 'let 
                                              (mlist (mcar params)))
                                              (let-body exp))]
          [else (mlist 'let 
                        (mlist (mcar params))
                        (make-let (mcdr params)))]))
  (make-let (mcadr exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (install-let*-syntax)
  (put-syntax! 'let eval-let)
  (put-syntax! 'let* eval-let*)
  (void))

(install-let*-syntax)