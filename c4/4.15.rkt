#lang racket

; (eval '(+ 1 2) (make-base-namespace))

; (eval (cons '* (list 5 5)) (make-base-namespace))

(define (run-forever)
  (run-forever))

(define (try-p)
  (if (halts? p p)
      (run-forever)
      'halted))

;;  When p = try

;;  (try try)
;;  (if (halts? try try)
;;      (run-forever)
;;      'halted)
;;
;;  When (halts? try try) is true, then the result is (run-forever).
;;  When (halts? try try) is false then the result is ‘halted.
;;
;;  Both answers are paradoxical and so there can be no such procedure halts? 
