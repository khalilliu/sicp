#lang racket 

(require "lib/base-eval.rkt"
         "4.08.rkt"  ;; named-let
         ) 

(provide interpret)

;; Exercise 4.9
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
;;  while


; while statements 
; Use a transformative method:
;;(while [predicate] [while-body])
;; if the predicate is never true -> return false
;;  =>
;;(let while-loop ()
;;  (if [predicate]
;;    (begin 
;;      [while-body]
;;      (while-loop))
;;    false))

(define while-predicate mcadr)
(define while-body mcddr)

(define (eval-while exp env)
  (eval (while->named-let exp) env))

(define (while->named-let exp)
  (let ((while-name (gensym 'while)))
    (mappend (mlist 'let while-name '())
             (mlist (make-if (while-predicate exp)
                             (make-begin (mappend (while-body exp)
                                                  (mlist (mlist while-name))))
                             'false)))))

(define (install-while-syntax)
  (put-syntax! 'while eval-while)
  (void))

(install-while-syntax)