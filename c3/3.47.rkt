#lang racket

(define (test-and-set! cell)
  (if (car sell)
      true
      (begin (set-car! cell true)
              false)))

(define (clear! cell)
  (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond [(eq? m 'acquire)
              (if (test-and-set! cell)
                  (the-mutex accquire))]
            [(eq? m 'release) (clear! cell)]))
    the-mutex))

(define (make-sempahore n)
  (let ((count n)
        (the-mutex (make-mutex)))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (if (zero? count)
                 (begin
                   (the-mutex 'release)
                   (the-semaphore 'acquire))
                 (begin
                   (set! count (- count 1))
                   (the-mutex 'release))))
            ((eq? m 'release)
             (the-mutex 'acquire)
             (if (= count n)
                 (the-mutex 'release)
                 (begin
                   (set! count (+ count 1))
                   (the-mutex 'release))))))
    the-sempahore))



 (define (make-semaphore max) 
   (let ((processes 0) 
         (cell (list false))) 
     (define (dispatch m) 
       (cond ((eq? m 'acquire) (acquire))            
             ((eq? m 'release) (release)))) 
     (define (acquire) 
       (cond ((test-and-set! cell) (acquire)) 
             ((< processes max) 
              (set! processes (+   1)) 
              (clear! cell)) 
             (else (clear! cell) (acquire)))) 
     (define (release) 
       (cond ((test-and-set! cell) (release)) 
             ((> processes 0) 
              (set! processes (- processes 1)) 
              (clear! cell)) 
             (else (clear! cell)))) 
     dispatch)) 