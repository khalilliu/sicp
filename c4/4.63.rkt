#lang racket

(require "lib/query-eval.rkt")

;; Exercise 4.63

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
   ))

(run-query '(assert! 
              (rule (grandson ?x ?z)
                    (and (son ?x ?y)
                         (son ?y ?z)))))
(run-query '(assert! 
              (rule (son ?x ?y)
                    (and (wife ?z ?y)
                         (son ?x ?z)))))

(run-query
 '(son ?x Lamech))
 
;;;  '((son Methushael Lamech))

(run-query '(grandson Cain ?y))
;;; '((grandson Cain Irad))

(run-query '(grandson ?y Methushael))
;;; '((grandson Irad Methushael))

(pretty-display (run-query '(son ?x ?y)))
;;; ((son Ada Jubal)
;;;  (son Ada Jabal)
;;;  (son Methushael Lamech)
;;;  (son Mehujael Methushael)
;;;  (son Irad Mehujael)
;;;  (son Enoch Irad)
;;;  (son Cain Enoch)
;;;  (son Adam Cain)
;;;  (son Methushael Ada))


(pretty-display(run-query '(grandson ?x ?y)))
;;; ((grandson Mehujael Lamech)
;;;  (grandson Irad Methushael)
;;;  (grandson Mehujael Ada)
;;;  (grandson Enoch Mehujael)
;;;  (grandson Cain Irad)
;;;  (grandson Adam Enoch)
;;;  (grandson Methushael Jubal)
;;;  (grandson Methushael Jabal))