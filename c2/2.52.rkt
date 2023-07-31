#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define wave 
  (segments->painter (list 
                      ;; ... 
                      (make-segment (make-vect 0.44 0.7) (make-vect 0.51 0.7))))) 
  

(paint wave)


(define (right-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (up-split painter (- n 1)))) 
        (below painter (beside smaller smaller))))) 

(define (corner-split painter n)
  (if (zero? n)
      painter
      (beside (below painter (up-split painter (- n 1)))
              (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))

(paint (corner-split einstein 5))