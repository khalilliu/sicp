#lang sicp

;; 
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 2)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (check-square (expmod base (/ exp 2) m) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (check-square n m)
  (if (and (not (or (= n 1) (= n (- m 1))))
           (= (remainder (* n n) m) 1))
      0
      (remainder (* n n) m)))

;; run test
(display (fast-prime? 561 200))
(newline)
(display (fast-prime? 1105 200))
(newline)
(display (fast-prime? 1729 200))
(newline)
(display (fast-prime? 2821 200))
(newline)
(display (fast-prime? 6601 200))