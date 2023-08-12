#lang racket

(require "lib/base-eval.rkt"
         "4.09.rkt"  ;; while
         )

(provide interpret)

;; Exercise 4.13
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
;;  and
;;  or
;;  cond
;;  special cond <test> => <recipient>
;;  let
;;  let*
;;  named let
;;  while

(define (frame-binding var frame)
  (let scan ((variables (mcar frame))
             (values (mcdr frame)))
    (cond [(or (null? variables) (null? values)) 'frame-binding-missing]
          [(eq? var (mcar variables)) (mcar values)]
          [else (scan (mcdr variables) (mcdr values))])))

(define (bound? var frame)
  (mmemq var (mcar frame)))

(define (set-binding-in-frame! var val frame)
  (let scan ((variables (mcar frame))
             (values (mcdr frame)))
    (cond [(or (null? variables) 
               (null? values)) 'frame-binding-missing]
          [(eq? var (mcar variables))
            (set-mcar! values val)]
          [else (scan (mcdr variables) (mcdr values))])))

(define (remove-binding-from-frame! var frame)
  (let scan ((variables (mcar frame)) (values (mcdr frame)))
    (cond [(or (null? variables)
               (null? values))
           (error "Unbound variable -- REMOVE-BINDING-FROM-FRAME!" var)]
          [(eq? var (mcar variables))
            (set-mcar! variables '())
            (set-mcar! values '())]
          [else (scan (mcdr variables) (mcdr values))])))

(define variable-to-unbind mcadr)
(define (eval-make-unbound! exp env)
  (remove-binding-from-frame! (variable-to-unbind exp)
                              (first-frame env)))

(define (install-unbinding-syntax)
  (put-syntax! 'make-unbound! eval-make-unbound!)
  (void))

(install-unbinding-syntax)

