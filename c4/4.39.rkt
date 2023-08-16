#lang racket

(require "lib/amb.rkt")

;; Exercise 4.39

(define (multiple-dwelling-1)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))))

(define (multiple-dwelling-2)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define tests 100)

(collect-garbage)
(collect-garbage)
(collect-garbage)
(time 
    (let next ((x tests))
        (cond [(zero? x) (void)]
              [else (multiple-dwelling-1)
                    (next (- x 1))])))
;;; cpu time: 138 real time: 143 gc time: 2

(collect-garbage)
(collect-garbage)
(collect-garbage)
(time 
    (let next ((x tests))
        (cond [(zero? x) (void)]
              [else (multiple-dwelling-2)
                    (next (- x 1))])))
;;; cpu time: 117 real time: 121 gc time: 2