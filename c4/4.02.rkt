#lang racket

;; a)

;; (eval '(define a 2)  env
;; -> (eval 'define env)
;;  -> (define-variable! (definition-variable 'define)
;;        (eval (definition-value 'define) env) env)
;;    -> (definition-variable 'define) => error

;; example
; (define (definition-value exp)
;   (if (symbol? (cadr exp))
;       (cadr exp)
;       (caddr exp)))

; (definition-value 'define)


;; b)

(require (rename-in "lib/base-eval.rkt"
          [application? base-application?]
          [operator base-operator]
          [operands base-operands]))

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (mcadr exp))
(define (operands exp) (mcddr exp))