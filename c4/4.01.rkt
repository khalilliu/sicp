#lang racket

(define no-operands? null?)
(define first-operand mcar)
(define rest-operand mcdr)

(define (list-of-values-l-to-r exps env)
  (if (no-operands? exps)
      '()
      (let ([left (eval (first-operand exps) env)]
            [right (list-of-values-l-to-r (rest-operand exps) env)])
          (mcons left right))))


(define (list-of-values-r-to-l exps env)
  (if (no-operands? exps)
      '()
      (let ([right (list-of-values-r-to-l (rest-operand exps) env)]
            [left (eval (first-operand exps) env)])
          (mcons left right))))