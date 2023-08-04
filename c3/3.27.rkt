#lang racket

(require "3.26b.rkt")

;; memorize fib


(define (memorize f)
  (let* ((table (make-table))
         (get (table 'lookup))
         (put (table 'insert!)))
    (lambda (x)
      (let ((prev-computed-result (get x)))
        (or prev-computed-result
            (let ((result (f x)))
              (put x result)
              result))))))

(define memo-fib
  (memorize (lambda (n)
              (cond [(= n 0) 0]
                    [(= n 1) 1]
                    [else (+ (memo-fib (- n 1))
                             (memo-fib (- n 2)))]))))


(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))


;; Test

(collect-garbage)
(time 
(begin 
  (fib 35)))
cpu time: 139 real time: 224 gc time: 0
;9227465


(collect-garbage)
(time 
(begin 
  (memo-fib 35)
  (memo-fib 35)
  (memo-fib 35)
  (memo-fib 35)
  (memo-fib 35)))
;cpu time: 0 real time: 0 gc time: 0
;9227465


(collect-garbage)
(time (memo-fib 200))
;cpu time: 3 real time: 8 gc time: 0
;280571172992510140037611932413038677189525

