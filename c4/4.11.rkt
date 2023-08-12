#lang racket

(require (except-in "lib/base-eval.rkt"
                    make-frame
                    add-binding-to-frame!
                    lookup-variable-value
                    set-variable-value!
                    define-variable!)
         "4.04.rkt"  ;; boolean expression
         "4.05.rkt"  ;; cond with =>
         "4.06.rkt"  ;; let
         "4.07.rkt"  ;; let*
         "4.08.rkt"  ;; named-let
         "4.09.rkt"  ;; while
         )


;; Exercise 4.11
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

(define (make-frame vars values)
  (map cons vars values))

(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (cond [(null? frame) (mcons binding frame)]
          [(null? (mcdr frame) (set-mcdr! frame binding)) 
           (set-mcdr! frame binding)]
          [else (add-binding! binding (mcdr frame))]))
  (add-binding! (mlist (mcons var val)) frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
        (if binding 
            (mcdr binding)
            (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
        (if binding
            (set-mcdr! binding val)
            (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (scan (first-frame env))))
  (env-loop env))


(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (assoc var frame)))
      (if binding 
          (set-mcdr! binding val)
          (add-binding-to-frame! var val frame))))