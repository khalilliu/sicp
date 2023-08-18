#lang racket

(require "lib/query-eval.rkt")

;; Exercise 4.62

(run-query '(assert! 
            (rule (last-pair (?x) (?x)))))

(run-query '(assert!
            (rule (last-pair (?x . ?y) (?z))
                  (last-pair ?y (?z)))))

(run-query '(last-pair (3) ?x))
;;; '((last-pair (3) (3)))

(run-query '(last-pair (1 2 3) ?x))
;;; '((last-pair (1 2 3) (3)))

(run-query '(last-pair (2 ?x) (3)))
;;; '((last-pair (2 3) (3)))

;;; won't returns because of infinite loop
;;; (run-query '(last-pair ?x (3)))
