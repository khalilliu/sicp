#lang racket

(require "lib/stream.rkt")

;; Exercise 3.72
;;
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

(define (stream-take n s)
  (cond [(stream-null? s) null]
        [(zero? n) null]
        [else (cons (stream-car s)
                    (stream-take (- n 1) (stream-cdr s)))]))

(define (stream-drop n s)
  (cond [(stream-null? s) the-empty-stream]
        [(zero? n) s]
        [else (stream-drop (- n 1) (stream-cdr s))]))

(define (sum-squares x)
  (let ((i (car x)) (j (cadr x)))
    (+ (* i i) (* j j))))

(define (triple-sum-squares)
  (define (filter-triple-sums integer-squares)
    (let* ((candiates (stream-take 3 integer-squares))
           (sums (map sum-squares candiates)))
          (cond [(and (= (car sums) (cadr sums))
                      (= (car sums) (caddr sums)))
                  (cons-stream (cons (car sums) candiates)
                               (filter-triple-sums (stream-drop 3 integer-squares)))]
                [else (filter-triple-sums (stream-cdr integer-squares))])))
  (filter-triple-sums (weighted-pairs
                        integers 
                        integers
                        sum-squares)))

(show-stream (triple-sum-squares) 5) 
; (325 (1 18) (6 17) (10 15))
; (425 (5 20) (8 19) (13 16))
; (650 (5 25) (11 23) (17 19))
; (725 (7 26) (10 25) (14 23))
; (845 (2 29) (13 26) (19 22))
