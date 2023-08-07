#lang racket

(require "lib/stream.rkt")



(define (sqrt-improve guess x)
  (define (average x y) (/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


(define (stream-limit stream tolerance)
  (cond [(stream-null? stream) null]
        [(stream-null? (stream-cdr stream)) (stream-car stream)]
        [else (let ((1st (stream-car stream)) (2nd (stream-car (stream-cdr stream))))
          (if (> tolerance (abs (- 1st 2nd)))
            2nd
            (stream-limit (stream-cdr stream) tolerance)
            ))]))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(show-stream (sqrt-stream 2) 10)
(newline)(newline)
(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.001)
(sqrt 2 0.0001)
