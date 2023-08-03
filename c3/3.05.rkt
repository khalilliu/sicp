#lang racket

(define (rand-update x)
  (random (expt 2 31)))

(define (random-init)
  (rand-update (random-seed 317)))

(define rand
  (let ((x (random-init)))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trails-passed)
    (cond [(= trials-remaining 0) (/ trails-passed trials)]
          [(experiment) 
            (iter (- trials-remaining 1) (+ trails-passed 1))]
          [else (iter (- trials-remaining 1) trails-passed)]))
  (iter trials 0))

(define (estimate-integral x1 y1 x2 y2 pred? trials)
  (monte-carlo trials 
    (lambda ()
      (pred? (random-in-range x1 x2) 
              (random-in-range y1 y2)))))

(define max-trials 10000)
(define radius 1000)

(define (estimate-pi trials)
  (exact->inexact
    (* 4
      (estimate-integral
        (- radius) (- radius) radius radius
        circle-test
        trials))))

(define (circle-test x y)
  (<= (+ (square x) (square y)) 
      (square radius)))


(estimate-pi max-trials)
;; => 3.1664