#lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.58

(define (pretty-query query) 
        (pretty-display (run-query query)))

(run-query
 '(assert!
   (rule (big-shot ?name ?division)
         (and (job ?name (?division . ?title))
              (or (not (supervisor ?name ?boss))
                  (and (supervisor ?name ?boss)
                       (not (job ?boss (?division . ?title-2)))))))))

(pretty-query '(big-shot ?who ?where))

;;; ((big-shot (Warbucks Oliver) administration)
;;;  (big-shot (Scrooge Eben) accounting)
;;;  (big-shot (Bitdiddle Ben) computer))