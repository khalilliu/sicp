#lang sicp

(define (f n)
  (cond ((< n 3) n)
         (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
         ))
  )


(define (f1 n) (g 0 n 0 1 2))

(define (g m n a b c) 
                     (if (= n m)
                         a
                         (g (+ m 1) n b c (+ c (* 2 b) (* 3 a)))
                         )
                     )

(f 2)
(f 3)
(f 22)
(f1 2)
(f1 3)
(f1 22)