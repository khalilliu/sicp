#lang racket

; (require "lib/base-eval.rkt"
;        "4.05.rkt")

; (provide interpret)

;; Exercise 4.6
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

;; let expression example:
;; （let ((<var1> <exp1>) (<var2> <exp2>)... (<varn> <expn>))
;;    <body>）


;; return (<exp1> <exp2> ... <expn>)
(define (let-initials exp)
  (mmap mcadr (mcadr exp)))

;; return (<var1> <var2> ... <varn>)
(define (let-parameters exp)
  (mmap mcar (mcadr exp)))

;; return (<body>)
(define (let-body exp)
  (mcddr exp))

;; return ((lambda (<var1> <var2> ... <varn>) <body>) 
;;              <exp1> <exp2> ... <expn>)
(define (let->combination exp)
  (mcons (make-lambda (let-parameters exp) (let-body exp))
         (let-initials exp)))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (install-let-syntax)
  (put-syntax! 'let eval-let)
  (void))

(install-let-syntax)

; (define let-exp '(let ((a 1) (b 1)) ('body)))

; (let-initials let-exp)