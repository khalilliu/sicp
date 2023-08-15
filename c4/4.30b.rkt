#lang racket 

;; Exercise 4.30b

(require "lib/base-eval.rkt")


(interpret '(define (for-each proc items)
              (if (null? items)
                  'done
                  (begin (proc (car items))
                         (for-each proc (cdr items))))))

(interpret '(for-each (lambda (x) (display x) (newline))
                      (list 57 321 88)))

(interpret '(define (p1 x)
              (set! x (cons x '(2)))
              x))

(interpret '(define (p2 x)
              (define (p e)
                e
                x)
              (p (set! x (cons x '(2))))))

(interpret '(p1 1))
(interpret '(p2 1))

;;; 57
;;; 321
;;; 88
;;; 'done
;;; '(1 2)
;;; '(1 2)


;  The reason is that p is a compound procedure and so eval-sequence is called to evaluate it. 
;  The argument e is bound to a thunk of the set! expression. 
;  In Cy’s version of the evaluator, each expression’s actual value is evaluated, 
;  forcing the value of the thunk and mutating the value of x. 
;  In the original evaluator eval is used to evaluate e. 
;  Since e is a thunk that eval does not force, the set! expression is not evaluated 
;  and the value of x is not mutated.