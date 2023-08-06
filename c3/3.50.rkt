#lang racket

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream null)
(define (stream-null? s) (null? s))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream 
        (apply proc (map stream-car argstreams))
        (apply stream-map (cons proc 
`                                (map stream-cdr argstreams))))))