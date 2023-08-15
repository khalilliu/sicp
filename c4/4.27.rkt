#lang racket 

(require "lib/lazy-eval.rkt") 

(provide interpret)


;; Exercise 4.27 - 
;;
;; The lazy evaluator 
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
;;  letrec

;;  (define count 0)
;;  (define (id x)
;;    (set! count (+ count 1))
;;    x)
;;  
;;  (define w (id (id 10)))
  
;;(interpret '(define w (id (id 10))))
;;; => 'ok
;;(interpret 'count)
;;; => 1
;;(interpret 'w)
;;; => 10
;;(interpret 'count)
;;; => 2  

;;; ;;  The first value of count is 1 is because when w is defined the value of w 
;;; ;;  is an application of the procedure id.
;;; ;;
;;; ;;  The expression (id (id 10)) has the operator id and a single operand (id 10).
;;; ;;
;;; ;;  For an application, the evaluator uses the actual value of the operand, 
;;; ;;  in this case id and delays the operands. 
;;; ;;  
;;; ;;  Evaluating the actual value of id causes the evaluation of the body of id once,
;;; ;;  which mutates the value of count to be 1.
;;; ;;
;;; ;;  Evaluating the value of w causes the delayed operand (id 10) to be forced, 
;;; ;;  mutating the value of count to become 2 and returning the value 10.
;;; ;;  

(interpret '(define (plus x y) (+ x y)))

(interpret '(define (id2 x) 
          (set! count (plus count 1)) 
          (+ x 0)) ; note (+ 0 x) as opposed to x 
) 

(interpret '(define count 0))
(interpret '(define w (id2 (id2 10)))) 
(interpret 'count) ;; 2
(interpret 'w) ;; 10
(interpret 'w) ;; '(evaluated-thunk 10)
(interpret 'count) ;; 2
(interpret 'count) ;; 2