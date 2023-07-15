#lang sicp

(define (get-coin-value coin-kind)
  (cond ((= coin-kind 1) 1)
        ((= coin-kind 2) 5)
        ((= coin-kind 3) 10)
        ((= coin-kind 4) 25)
        ((= coin-kind 5) 50)
        )
  )

(define (count-change amount) (cc amount 5))

(define (cc amount coin-kind)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= coin-kind 0)) 0)
        (else (+ (cc amount (- coin-kind 1))
                 (cc (- amount (get-coin-value coin-kind)) coin-kind)))
        )
  )

(count-change 100)