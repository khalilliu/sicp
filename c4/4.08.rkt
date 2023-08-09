#lang racket 

(require "lib/base-eval.rkt"
         "ex4.07.rkt"  ;; let*
         ) 

;; Exercise 4.8
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
;;  named let

; Let statements
(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (mcadr exp))))

; Let selectors
(define (let-initials exp)   (mmap mcadr (mcadr exp)))
(define (let-parameters exp) (mmap mcar (mcadr exp)))
(define named-let-identifier mcar)
(define let-body mcddr)


(define (named-let->combination exp)
  (let ((precedure-name (named-let-identifier exp)))
      (make-begin 
        (mlist 'define precedure-name
              (make-lambda 
                (let-parameters exp)
                (let-body exp)))
        ; apply the procedure with the initial values given by the let expression
        (mcons precedure-name (let-initials exp)))
  ))


; a let is syntactic sugar for
;   ((lambda (params) (body)) values)
(define (let+named->combination exp)
  (if (named-let? exp)
      (named-let->combination (mcdr exp))
      (mcons (make-lambda (let-parameters exp) (let-body exp))
             (let-initials exp))))

(define (eval-named-let exp env)
  (eval (let+named->combination exp) env))

(define (install-named-let-syntax)
  (put-syntax! 'let eval-named-let)
  (void))

(install-named-let-syntax)