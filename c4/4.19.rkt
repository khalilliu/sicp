#lang racket

;; in js it if use let , it will be error
;; but use var , it will be NAN
;; in golang, it will be 16
  ;  (let ((a 1))
  ;    (define (f x)
  ;      (define b (+ a x))
  ;      (define a 5)
  ;      (+ a b))
  ;    (f 10))

  ;; ;;    ERROR! +: expects type <number> as 1st argument, given: #<undefined>; other arguments were: 10