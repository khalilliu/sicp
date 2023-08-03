#lang racket

(require scheme/mpair)
(define (last-pair x)
  (cond ((null? (mcdr x)) x)
        (else (last-pair (mcdr x)))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)


(define z (mlist 'a 'b 'c))
(last-pair z) 
;; (mcons 'c '())

(make-cycle z) 
;; #0=(mcons 'a (mcons 'b (mcons 'c #0#)))
