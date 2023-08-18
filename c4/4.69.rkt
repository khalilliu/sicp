#lang racket

(require "lib/query-eval-loop-detector.rkt")

;; Exercise 4.69

;; the database
(make-database
 '((son Adam Cain)
   (son Cain Enoch)
   (son Enoch Irad)
   (son Irad Mehujael)
   (son Mehujael Methushael)
   (son Methushael Lamech)
   (wife Lamech Ada)
   (son Ada Jabal)
   (son Ada Jubal)
   (rule (grandson ?x ?y)
         (and (son ?x ?z)
              (son ?z ?y)))
   (rule (son ?x ?y)
         (and (wife ?z ?y)
              (son ?x ?z)))))


(run-query
 ; base case
 '(assert!
   (rule (ends-in-grandson (grandson)))))

(run-query
 '(assert!
   (rule (ends-in-grandson (?greats . ?rel))
          (ends-in-grandson ?rel))))

(run-query '(ends-in-grandson (great great great grandson)))
;;; '((ends-in-grandson (great great great grandson)))

(run-query
 '(assert!
   (rule ((great . ?rel) ?x ?y) 
         (and (ends-in-grandson ?rel) 
              (son ?x ?other) 
              (?rel ?other ?y)))))


;;; insert 
(run-query
 '(assert!
   ((great grandson) Adam Irad)))

(run-query '((great grandson) ?g ?ggs))
;;; '(((great grandson) Adam Irad))
(run-query '(?rel Adam Irad))
;;; '(((great grandson) Adam Irad))

(run-query '(?rel Adam ?y))
;;; '(((great grandson) Adam Irad) (son Adam Cain) (grandson Adam Enoch))
(run-query '(?rel ?x Irad))
;;; '(((great grandson) Adam Irad) (son Enoch Irad) (grandson Cain Irad))

(run-query '(?rel ?x Mehujael))