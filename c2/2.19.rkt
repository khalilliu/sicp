#lang racket

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 40 20 10 5 2 1 0.5))

(define (no-more? items)
  (null? items))

(define (first-denomination items)
  (car items))

(define (except-first-denomination items)
  (cdr items))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))]))



;; test
(cc 100 us-coins)

(cc 100 (reverse us-coins))
;; 292

(cc 100 uk-coins)

(cc 100 (reverse uk-coins))
;; 111023