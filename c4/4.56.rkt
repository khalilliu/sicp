#lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.56

(define (pretty-query query) 
        (pretty-display (run-query query)))

;;; a
(pretty-query '(and (supervisor ?name (Bitdiddle Ben))
                    (address ?name ?address)))

;;; ((and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;;;       (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
;;;  (and (supervisor (Fect Cy D) (Bitdiddle Ben))
;;;       (address (Fect Cy D) (Cambridge (Ames Street) 3)))
;;;  (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;;;       (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))))


;;; b
(pretty-query '(and (salary (Bitdiddle Ben) ?ben-salary)
                    (salary ?name ?salary)
                    (lisp-value < ?salary ?ben-salary)))

;;; ((and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Aull DeWitt) 25000)
;;;       (lisp-value < 25000 60000))
;;;  (and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Cratchet Robert) 18000)
;;;       (lisp-value < 18000 60000))
;;;  (and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Reasoner Louis) 30000)
;;;       (lisp-value < 30000 60000))
;;;  (and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Tweakit Lem E) 25000)
;;;       (lisp-value < 25000 60000))
;;;  (and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Fect Cy D) 35000)
;;;       (lisp-value < 35000 60000))
;;;  (and (salary (Bitdiddle Ben) 60000)
;;;       (salary (Hacker Alyssa P) 40000)
;;;       (lisp-value < 40000 60000)))

;;; c

(pretty-query '(and (supervisor ?name ?supervisor)
                    (job ?supervisor ?supervisor-job)
                    (not (job ?supervisor (computer . ?title)))))

;;; ((and (supervisor (Aull DeWitt) (Warbucks Oliver))
;;;       (job (Warbucks Oliver) (administration big wheel))
;;;       (not (job (Warbucks Oliver) (computer . ?title))))
;;;  (and (supervisor (Cratchet Robert) (Scrooge Eben))
;;;       (job (Scrooge Eben) (accounting chief accountant))
;;;       (not (job (Scrooge Eben) (computer . ?title))))
;;;  (and (supervisor (Scrooge Eben) (Warbucks Oliver))
;;;       (job (Warbucks Oliver) (administration big wheel))
;;;       (not (job (Warbucks Oliver) (computer . ?title))))
;;;  (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;;;       (job (Warbucks Oliver) (administration big wheel))
;;;       (not (job (Warbucks Oliver) (computer . ?title)))))