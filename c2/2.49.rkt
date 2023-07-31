#lang sicp

(#%require sicp-pict)

;; Exercise 2.49a
(define outline 
  (segments->painter 
   (list 
    (segment (vect 0.0 0.0) (vect 0.0 1.0)) 
    (segment (vect 0.0 0.0) (vect 1.0 0.0)) 
    (segment (vect 0.0 1.0) (vect 1.0 1.0)) 
    (segment (vect 1.0 0.0) (vect 1.0 1.0)))))

;; (paint outline)

;; Exercise 2.49b
(define x-painter
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 1.0 1.0))
    (segment (vect 0.0 1.0) (vect 1.0 0.0)))))

;; (paint x-painter)

;; Exercise 2.49c
(define diamond 
  (segments->painter 
   (list 
    (segment (vect 0.0 0.5) (vect 0.5 1.0)) 
    (segment (vect 0.5 1.0) (vect 1.0 0.5)) 
    (segment (vect 1.0 0.5) (vect 0.5 0.0)) 
    (segment (vect 0.5 0.0) (vect 0.0 0.5)))))

;; (paint diamond)

;; Exercise 2.49d
;; code from `http://community.schemewiki.org/?SophiaG`
(define wave 
  (segments->painter
   (list 
    (make-segment (make-vect .25 0) (make-vect .35 .5)) 
    (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
    (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
    (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
    (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
    (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
    (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
    (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
    (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
    (make-segment (make-vect .35 .85) (make-vect .4 1)) 
    (make-segment (make-vect .4 1) (make-vect .6 1)) 
    (make-segment (make-vect .6 1) (make-vect .65 .85)) 
    (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
    (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
    (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
    (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
    (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
    (make-segment (make-vect .6 .45) (make-vect .75 0)) 
    (make-segment (make-vect .75 0) (make-vect .6 0)) 
    (make-segment (make-vect .6 0) (make-vect .5 .3)) 
    (make-segment (make-vect .5 .3) (make-vect .4 0)) 
    (make-segment (make-vect .4 0) (make-vect .25 0)) 
    )))

(paint wave)