#lang racket

(define square (lambda (x) (* x x)))

(define inc (lambda (x) (+ x 1)))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Recursive process 
(define (int-to-church n)
  (if (= n 0)
      zero
      (add-1 (int-to-church (- n 1)))))

; (define one (int-to-church 1))

; (define two (int-to-church 2))

; (((add-1 one) square) 2)

; (((add one one) square) 2)

;; Iterative process 
(define (int-to-church-iter n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (add-1 result))))
  (iter 0 zero))

(define one (int-to-church-iter 1))

(define two (int-to-church-iter 2))

(((add-1 one) square) 2)

(((add two two) inc) 2)