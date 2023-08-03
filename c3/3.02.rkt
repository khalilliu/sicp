#lang racket


(define (make-monitored f)
  (let ((count 0))
       (lambda (x)
        (cond [(eq? x 'how-many-calls?) count]
              [(eq? x 'reset-count) 
                (set! count 0)
                count]
              [else 
                (set! count (add1 count))
                (f x)]))))



(define s (make-monitored sqrt))
(s 100)
(s 64)
(s 'how-many-calls?)
(s 100)
(s 64)
(s 'reset-count)
(s 'how-many-calls?)




