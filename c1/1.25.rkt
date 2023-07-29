#lang sicp

(define (fast-prime? n times)
  (cond ((= times 0) 1)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else 0)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))

(display (fast-prime? 561 20))
(newline)
(display (fast-prime? 1105 20))
(newline)
(display (fast-prime? 1729 20))
(newline)
(display (fast-prime? 2821 20))
(newline)
(display (fast-prime? 6601 20))