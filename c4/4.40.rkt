#lang racket

(require "lib/amb.rkt")

;; Exercise 4.40

(define (multiple-dwelling-original)
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


(define (multiple-dwelling)
    (let* ((fletcher (amb 2 3 4))
           (cooper (an-element-of (filter (lambda (x)
                                          (not (= (abs (- fletcher x)) 1)))
                                          (list 2 3 4 5))))
            (miller (an-element-of (filter (λ (x)
                                          (> x cooper))
                                        (list 1 2 3 4 5))))
            (smith (an-element-of (filter (λ (x)
                                         (not (= (abs (- fletcher x)) 1)))
                                       (list 1 2 3 4 5))))
            (baker (amb 1 2 3 4)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (multiple-dwelling-2)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

(define tests 100)

(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) (void))
         (else (multiple-dwelling-original)
               (next (- x 1))))))

(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) (void))
         (else (multiple-dwelling)
               (next (- x 1))))))

(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) (void))
         (else (multiple-dwelling-2)
               (next (- x 1))))))




;;; cpu time: 154 real time: 166 gc time: 8
;;; cpu time: 16 real time: 17 gc time: 0
;;; cpu time: 7 real time: 7 gc time: 0