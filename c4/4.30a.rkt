#lang racket 

;; Exercise 4.30a

(require "lib/lazy-eval.rkt")

(interpret '(define (for-each proc items)
              (if (null? items)
                  'done
                  (begin (proc (car items))
                         (for-each proc (cdr items))))))


(interpret '(for-each (lambda (x) (display x) (newline))
                      (list 57 321 88)))


;;; In begin expression, every expression will be evaluated using eval,
;;; and display is primitive function, it will call force-it to get x. 