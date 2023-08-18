#lang racket

(require "lib/query-eval-loop-detector.rkt")

;; Exercise 4.68

(run-query '(assert! (rule (append-to-form () ?y ?y))))
(run-query '(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                           (append-to-form ?v ?y ?z))))

;;; base-case
(run-query '(assert! (rule (reverse () ()))))

(run-query '(assert! (rule (reverse (?h . ?t) ?y)
                           (and (reverse ?t ?r)  
                                (append-to-form ?r (?h) ?y)))))



(run-query '(reverse (1 2 3) ?x))

(run-query '(reverse ?x (1 2 3)))

;;;  this will go into an endless loop because of 
;;; the recursive call to reverse on an unbound value
