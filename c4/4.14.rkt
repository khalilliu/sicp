#lang racket 

(require "lib/base-eval.rkt"
         "4.04.rkt"  ;; boolean expression
         "4.05.rkt"  ;; cond with =>
         "4.06.rkt"  ;; let
         "4.07.rkt"  ;; let*
         "4.08.rkt"  ;; named-let
         "4.09.rkt"  ;; while
         ) 


(provide interpret)


(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval (exp->mlist input) the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object))
  (newline))


;; we can use the following to test the interpreter
; (driver-loop)


; (exp->mlist '(define (append x y)
;   (if (null? x)
;       y
;       (cons (car x) (append (cdr x) y))))
; )

; '(define (square x) (* x x))

 (interpret '(map + '1 '2))

; (define exp (exp->mlist '(map + '(1 2) '(3 4))))
; (display (operator exp))
; (display (list-of-values (operands exp) the-global-environment))


;  But when the evaluator tries to evaluate map, for example 
;;    (map + '(1 2) '(3 4))  - it calls
;;    
;;    (apply (eval (operator exp) env)
;;           (list-of-values (operands exp) env))
;;
;;    (eval (operator exp) env) – returns scheme's own map procedure, but list-of-values 
;;    doesn't produce a result which scheme's map can use. list-of-values returns a list 
;;    made by calling eval with each operand. 
;;    The first operand is + which eval treats as a variable, since it is a symbol
;;    when it looks up the value in the environment the value returned is (primitive +) 
;;    which scheme's built in map can't use.
;;    