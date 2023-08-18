#lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.57

(define (pretty-query query) 
        (pretty-display (run-query query)))

(run-query '(assert! 
  (rule (can-replace ?person-1 ?person-2)
        (and (job ?person-1 ?job-1)
             (job ?person-2 ?job-2)
             (not (same ?person-1 ?person-2))
             (or (same ?job-1 ?job-2)
                 (can-do-job ?job-1 ?job-2))))))

;;; a
(pretty-query '(can-replace ?who (Fect Cy D)))
;;; ((can-replace (Hacker Alyssa P) (Fect Cy D))
;;;  (can-replace (Bitdiddle Ben) (Fect Cy D)))


;;; b
(pretty-query '(and (can-replace ?x ?y)
                    (salary ?x ?x-salary)
                    (salary ?y ?y-salary)
                    (lisp-value > ?y-salary ?x-salary)))

;;; ((and (can-replace (Fect Cy D) (Hacker Alyssa P))
;;;       (salary (Fect Cy D) 35000)
;;;       (salary (Hacker Alyssa P) 40000)
;;;       (lisp-value > 40000 35000))
;;;  (and (can-replace (Aull DeWitt) (Warbucks Oliver))
;;;       (salary (Aull DeWitt) 25000)
;;;       (salary (Warbucks Oliver) 150000)
;;;       (lisp-value > 150000 25000)))