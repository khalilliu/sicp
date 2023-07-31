#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split op1 op2)
  (define (tmp-split painter n)
    (if (zero? n)
        painter
        (let ((smaller (tmp-split painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  tmp-split)

(define right-split (split beside below))

(paint (right-split einstein 2))

(define up-split (split below beside))

(paint (up-split einstein 2)) 