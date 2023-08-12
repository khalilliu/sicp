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

(provide interpret)

;; Exercise 4.12
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

(define (make-frame vars values) (mmap mcons vars values))
(define (frame-binding var frame) (massoc var frame))
(define bound? frame-binding)

(define (add-binding-to-frame! var val frame)
  (define (add-binding! binding frame)
    (cond [(null? (mcdr frame)) (set-mcdr! frame binding)]
          [else (add-binding! binding (mcdr frame))]))
  (add-binding! (mlist (mcons var val)) frame))

(define (set-binding-in-frame! var val frame)
  (set-mcdr! (frame-binding var frame) val))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (mcdr (frame-binding var frame))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
            (if (bound? var frame)
                (set-binding-in-frame! var val frame)
                (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (bound? var frame)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! var val frame))))

