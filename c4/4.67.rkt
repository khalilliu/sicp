#lang racket
(require "lib/query-eval-loop-detector.rkt")


;; Exercise 4.67


(pretty-display (run-query '(outranked-by ?person ?who)))
;;; ((outranked-by (Aull DeWitt) (Warbucks Oliver))
;;;  (outranked-by (Cratchet Robert) (Warbucks Oliver))
;;;  (outranked-by (Cratchet Robert) (Scrooge Eben))
;;;  (outranked-by (Reasoner Louis) (Bitdiddle Ben))
;;;  (outranked-by (Scrooge Eben) (Warbucks Oliver))
;;;  (outranked-by (Tweakit Lem E) (Warbucks Oliver))
;;;  (outranked-by (Bitdiddle Ben) (Warbucks Oliver))
;;;  (outranked-by (Reasoner Louis) (Warbucks Oliver))
;;;  (outranked-by (Reasoner Louis) (Hacker Alyssa P))
;;;  (outranked-by (Fect Cy D) (Warbucks Oliver))
;;;  (outranked-by (Tweakit Lem E) (Bitdiddle Ben))
;;;  (outranked-by (Hacker Alyssa P) (Warbucks Oliver))
;;;  (outranked-by (Fect Cy D) (Bitdiddle Ben))
;;;  (outranked-by (Hacker Alyssa P) (Bitdiddle Ben)))



(run-query
 '(assert!
   (rule (outranked-by-loop ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by-loop ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager))))))
(pretty-display (run-query '(outranked-by-loop ?person ?who)))
;;; ((outranked-by-loop (Aull DeWitt) (Warbucks Oliver))
;;;  (outranked-by-loop (Cratchet Robert) (Warbucks Oliver))
;;;  (outranked-by-loop (Cratchet Robert) (Scrooge Eben))
;;;  (outranked-by-loop (Tweakit Lem E) (Warbucks Oliver))
;;;  (outranked-by-loop (Scrooge Eben) (Warbucks Oliver))
;;;  (outranked-by-loop (Reasoner Louis) (Bitdiddle Ben))
;;;  (outranked-by-loop (Bitdiddle Ben) (Warbucks Oliver))
;;;  (outranked-by-loop (Fect Cy D) (Warbucks Oliver))
;;;  (outranked-by-loop (Reasoner Louis) (Hacker Alyssa P))
;;;  (outranked-by-loop (Hacker Alyssa P) (Warbucks Oliver))
;;;  (outranked-by-loop (Tweakit Lem E) (Bitdiddle Ben))
;;;  (outranked-by-loop (Fect Cy D) (Bitdiddle Ben))
;;;  (outranked-by-loop (Hacker Alyssa P) (Bitdiddle Ben)))


(run-query
 '(assert! (married Minnie Mickey)))

(run-query
 '(assert! (rule (married ?x ?y)
                 (married ?y ?x))))

(pretty-display (run-query '(married Mickey ?who)))
;;; ((married Mickey Minnie) (married Mickey Minnie))