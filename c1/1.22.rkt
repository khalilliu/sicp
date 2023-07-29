#lang sicp

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (smallest-divisor n) (find-divisor n 2))
(define (square x) (* x x))

(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))
        )
  )

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (search-for-primes n count)
  (cond ((= count 0) (values))
        ((even? n) (search-for-primes (+ n 1) count))
        (else (if (timed-prime-test n)
                  (search-for-primes (+ n 2) (- count 1))
                  (search-for-primes (+ n 2) count)))
        ))



(search-for-primes 1000 3) ;;1009, 1013, 1019
(newline)
(search-for-primes 10000 3) ;;10007, 10009, 10037
(newline)
(search-for-primes 100000 3) ;; 100003, 100019, 100043
(newline)
(search-for-primes 1000000 3) ;; 1000003, 1000033, 1000037
(newline)

;;These are still too fast so using larger numbers
(search-for-primes 100000000 2); approximately 1000
(newline)
(search-for-primes 1000000000 2); approximately 3000
(newline)
(search-for-primes 10000000000 2); approximately 10000
