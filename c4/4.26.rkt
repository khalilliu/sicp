#lang racket

(require (except-in "lib/base-eval.rkt"
                    lookup-variable-value)
         "4.04.rkt"  ;; boolean expression
         "4.05.rkt"  ;; cond with =>
         "4.06.rkt"  ;; let
         "4.07.rkt"  ;; let*
         "4.08.rkt"  ;; named-let
         "4.09.rkt"  ;; while
         "4.13.rkt"  ;; unbinding
         "4.16a.rkt"  ;; internal binding
         "4.20a.rkt"  ;; letrec)
)

(provide interpret)



;; Exercise 4.26
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
;;  letrec
;;  unless

(define (eval-unless exp env)
  (eval (make-if (unless-conditional exp)
                 (unless-exceptional exp)
                 (unless-usual exp))
        env))

(define (unless-conditional exp)
  (mcadr exp))
(define (unless-usual exp)
  (mcaddr exp))
(define (unless-exceptional exp)
  (if (not (null? (mcadddr exp)))
      (mcadddr exp)
      'false))


(define (install-unless-syntax)
  (put-syntax! 'unless eval-unless)
  (void))

(install-unless-syntax)

;; test

;;; (interpret '(define a 12))
;;; (interpret '(unless (equal? a 12)
;;;         (display "a is 12")
;;;         (display "2 than 1")))