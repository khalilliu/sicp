#lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.59

(define (pretty-query query) 
        (pretty-display (run-query query)))


(run-query '(assert!
                (rule (uniq-lives-near ?person-1 ?person-2)
                        (and (address ?person-1 (?town . ?rest-1))
                             (address ?person-2 (?town . ?rest-2))
                             (lisp-value person<? ?person-1 ?person-2)))))



(pretty-query '(lives-near ?person1 ?person2))
;;; ((lives-near (Aull DeWitt) (Reasoner Louis))
;;;  (lives-near (Aull DeWitt) (Bitdiddle Ben))
;;;  (lives-near (Reasoner Louis) (Aull DeWitt))
;;;  (lives-near (Reasoner Louis) (Bitdiddle Ben))
;;;  (lives-near (Hacker Alyssa P) (Fect Cy D))
;;;  (lives-near (Fect Cy D) (Hacker Alyssa P))
;;;  (lives-near (Bitdiddle Ben) (Aull DeWitt))
;;;  (lives-near (Bitdiddle Ben) (Reasoner Louis)))

(pretty-query '(uniq-lives-near ?person1 ?person2))
;;; ((uniq-lives-near (Aull DeWitt) (Reasoner Louis))
;;;  (uniq-lives-near (Aull DeWitt) (Bitdiddle Ben))
;;;  (uniq-lives-near (Fect Cy D) (Hacker Alyssa P))
;;;  (uniq-lives-near (Bitdiddle Ben) (Reasoner Louis)))