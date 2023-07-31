#lang racket


(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (flip-horiz painter) 
  (transform-painter painter 
                     (make-vect 1.0 0.0) 
                     (make-vect 0.0 0.0) 
                     (make-vect 1.0 1.0))) 
  
(define (repeated fn t) 
  (if (= t 1) 
      fn 
      (lambda (x) 
        (fn ((repeated fn (- t 1)) 
             x))))) 
  
(define (rotate180 painter) 
  ((repeated  rotate90 2) painter)) 
  
(define (rotate270 painter) 
  ((repeated rotate90 3) painter)) 

