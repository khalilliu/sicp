#lang racket

(require "lib/amb.rkt")

;; Exercise 4.38

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)

(try-again)
(try-again)
(try-again)
(try-again)
(try-again)
(try-again)


;;; ((baker 1) (cooper 2) (fletcher 3) (miller 4) (smith 5))
;;; ((baker 1) (cooper 2) (fletcher 3) (miller 5) (smith 4))
;;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;;; ((baker 1) (cooper 3) (fletcher 2) (miller 5) (smith 4))
;;; ((baker 1) (cooper 3) (fletcher 4) (miller 5) (smith 2))
;;; ((baker 2) (cooper 3) (fletcher 4) (miller 5) (smith 1))