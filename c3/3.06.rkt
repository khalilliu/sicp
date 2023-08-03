#lang racket

(define initial-seed 317)
(define (rand-update x)
  (random (expt 2 31)))

(define (random-init)
  (rand-update (random-seed initial-seed)))

(define rand
  (let ((x (random-init)))
    (lambda (action)
      (cond ((eq? action 'generate) 
             (set! x (rand-update x))
             x)
            ((eq? action 'reset) 
             (lambda (new-seed) 
               (rand-update (random-seed new-seed))))))))


