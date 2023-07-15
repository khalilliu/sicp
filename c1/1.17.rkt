#lang sicp

(define (pow a b) (qmi a b 1))
(define (even? a) (= (remainder a 2) 0))

(define (qmi a b res) 
                       (cond ((= b 0) res)
                             ((even? b) (qmi (* a a) (/ b 2) res))
                             (else (qmi (* a a) (/ (- b 1) 2) (* res a)))
                             )
                       )

(pow 2 10)