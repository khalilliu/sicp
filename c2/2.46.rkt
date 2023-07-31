#lang racket


  
(define (make-vect x y) (cons x y)) 
(define (xcor-vect vec) (car vec)) 
(define (ycor-vect vec) (cdr vec)) 


(define (eq-vect? v1 v2) 
  (and (= (xcor-vect v1) (xcor-vect v2)) 
       (= (ycor-vect v1) (ycor-vect v2)))) 
  
(define (add-vect v1 v2) 
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
             (+ (ycor-vect v1) (ycor-vect v2)))) 
(define (sub-vect v1 v2) 
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))