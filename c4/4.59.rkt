#lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.59

(define (pretty-query query) 
        (pretty-display (run-query query)))


(run-query '(assert! (meeting accounting (Monday 9am))))
(run-query '(assert! (meeting administration (Monday 10am))))
(run-query '(assert! (meeting computer (Wednesday 3pm))))
(run-query '(assert! (meeting administration (Friday 1pm))))
(run-query '(assert! (meeting whole-company (Wednesday 4pm))))


;;; a
(pretty-query '(meeting ?where (Friday ?time)))
;;; ((meeting administration (Friday 1pm)))

;;; b
(run-query '(assert!
                (rule (meeting-time ?person ?day-and-time)
                      (or (and (job ?person (?division . ?title))
                               (meeting ?division ?day-and-time))
                          (meeting whole-company ?day-and-time)))))

;;; c
(pretty-query '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))

;;; ((meeting-time (Hacker Alyssa P) (Wednesday 3pm))
;;;  (meeting-time (Hacker Alyssa P) (Wednesday 4pm)))

