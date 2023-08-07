#lang racket

(require "lib/stream.rkt")


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car 
                            (merge-weighted (stream-cdr s1) 
                                            s2 
                                            weight))
               (cons-stream s2car 
                            (merge-weighted s1 
                                            (stream-cdr s2) 
                                            weight)))))))

(define (weighted-pairs s t weight) 
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))



(define (ramujan-numbers)
  (define (sum-cubed x)
    (let ((i (car x)) (j (cadr x)))
      (+ (* i i i) (* j j j))))
  (define (ramujans all-sum-cubes)
    (let* ((current (stream-car all-sum-cubes))
           (next (stream-car (stream-cdr all-sum-cubes)))
           (ramujan-candidate (sum-cubed current)))
      (cond [(= ramujan-candidate (sum-cubed next)) 
              (cons-stream (list ramujan-candidate current next)
                           (ramujans (stream-cdr (stream-cdr all-sum-cubes))))]
            [else (ramujans (stream-cdr all-sum-cubes))])))
  (ramujans (weighted-pairs integers 
                            integers 
                            sum-cubed)))

(show-stream (ramujan-numbers) 10)
; (1729 (1 12) (9 10))
; (4104 (2 16) (9 15))
; (13832 (2 24) (18 20))
; (20683 (10 27) (19 24))
; (32832 (4 32) (18 30))
; (39312 (2 34) (15 33))
; (40033 (9 34) (16 33))
; (46683 (3 36) (27 30))
; (64232 (17 39) (26 36))
; (65728 (12 40) (31 33))