#lang sicp

(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (my-car z) (z 'car))

(define (my-cdr z) (z 'cdr))

(define (my-set-car! z nval)
  ((z 'set-car!) nval)
  z)

(define (my-set-cdr! z nval)
  ((z 'set-cdr!) nval)
  z)

(define x (my-cons 1 2))

(define z (my-cons x x))

(my-set-car! (my-cdr z) 17)

(my-car x)

(my-car (my-cdr z))