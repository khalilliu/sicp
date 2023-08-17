#lang racket

(require "lib/ramb-eval.rkt")

;; Exercise 4.51

(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items)))))

(interpret '(define count 0))

(driver-loop)


;;; ;;; Ramb-Eval input:
;;; (let ((x (an-element-of '(a b c)))
;;;      (y (an-element-of '(a b c))))
;;;  (set! count (+ count 1))
;;;  (require (not (eq? x y)))
;;;  (list x y count))

;;; ;;; Starting a new problem 
;;; ;;; Ramb-Eval value:
;;; (a b 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (a c 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (b a 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (b c 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (c a 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (c b 1)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (let ((x (an-element-of (quote (a b c)))) (y (an-element-of (quote (a b c))))) (set! count (+ count 1)) (require (not (eq? x y))) (list x y count))



;;; ;;; Ramb-Eval input:
;;; (let ((x (an-element-of '(a b c)))
;;;      (y (an-element-of '(a b c))))
;;;  (permanent-set! count (+ count 1))
;;;  (require (not (eq? x y)))
;;;  (list x y count))

;;; ;;; Starting a new problem 
;;; ;;; Ramb-Eval value:
;;; (a b 2)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (a c 3)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (b a 4)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (b c 6)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (c a 7)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; Ramb-Eval value:
;;; (c b 8)

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (let ((x (an-element-of (quote (a b c)))) (y (an-element-of (quote (a b c))))) (permanent-set! count (+ count 1)) (require (not (eq? x y))) (list x y count))

;;; ;;; Ramb-Eval input:
;;; try-again

;;; ;;; There is no current problem
