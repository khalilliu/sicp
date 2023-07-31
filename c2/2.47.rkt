#lang racket


(define (make-frame origin edge1 edge2) 
  (list origin edge1 edge2)) 
(define (frame-origin f) (car f)) 
(define (frame-edge1 f) (cadr f)) 
(define (frame-edge2 f) (caddr f))


;; Test

(define f (make-frame 1 2 3)) 
(frame-origin f) 
(frame-edge1 f) 
(frame-edge2 f) 