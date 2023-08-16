#lang racket

(require "lib/amb.rkt")

;; Exercise 4.41

(define (multiple-dwelling-original)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (present-solution (list baker cooper fletcher miller smith)))))))

(define (present-solution solution)
  (map list 
       '(baker cooper fletcher miller smith)
       solution))

(define (multiple-dwelling)
    
  (define (invalid-solution? permutation)
    (let ((baker (first permutation))
          (cooper (second permutation))
          (fletcher (third permutation))
          (miller (fourth permutation))
          (smith (fifth permutation)))
      (and (not (= baker 5))
           (not (= cooper 1))
           (not (= fletcher 5))
           (not (= fletcher 1))
           (> miller cooper)
           (not (= (abs (- smith fletcher)) 1))
           (not (= (abs (- fletcher cooper)) 1)))))
  
  (map present-solution
       (filter invalid-solution?
               (permutations (list 1 2 3 4 5)))))


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

;;; cpu time: 8 real time: 9 gc time: 0
;;; cpu time: 2 real time: 2 gc time: 0