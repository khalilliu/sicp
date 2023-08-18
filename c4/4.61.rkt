#lang racket

(require "lib/query-eval.rkt")

(define (pretty-query query) 
        (pretty-display (run-query query)))

(run-query 
  '(assert! 
    (rule (append-to-form () ?y ?y))))

(run-query 
  '(assert! 
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))))

;;; (pretty-query '(append-to-form ?x ?y (a b c d)))
;;; ((append-to-form (a b c d) () (a b c d))
;;;  (append-to-form () (a b c d) (a b c d))
;;;  (append-to-form (a) (b c d) (a b c d))
;;;  (append-to-form (a b) (c d) (a b c d))
;;;  (append-to-form (a b c) (d) (a b c d)))

;; Exercise 4.61
(run-query 
  '(assert! 
    (rule (?x next-to ?y in (?x ?y . ?u)))))

(run-query 
  '(assert!
    (rule (?x next-to ?y in (?v . ?z))
          (?x next-to ?y in ?z))))

;;; (pretty-query '(?x next-to ?y in (1 (2 3) 4)))
;;; (((2 3) next-to 4 in (1 (2 3) 4)) 
;;;  (1 next-to (2 3) in (1 (2 3) 4)))

;;; (pretty-query '(?x next-to 1 in (2 1 3 1)))
;;; ((3 next-to 1 in (2 1 3 1)) 
;;;  (2 next-to 1 in (2 1 3 1)))


(run-query '(assert! 
              (rule (merge-to-form ?x () ?x))))

(run-query '(assert! 
              (rule (merge-to-form () ?x ?x))))

(run-query '(assert! 
              (rule (merge-to-form 
                      (?a . ?x) (?b . ?y) (?b . ?z))
                    (and (merge-to-form (?a . ?x) ?y ?z)
                         (lisp-value > ?a ?b)))))

(run-query '(assert! 
              (rule (merge-to-form 
                      (?a . ?x) (?b . ?y) (?a . ?z))
                    (and (merge-to-form ?x (?b . ?y) ?z)
                         (lisp-value > ?b ?a)))))

(pretty-display (run-query '(merge-to-form (1 6 ?d) (2 5 9) (?a 2 5 6 8 9))))