#lang racket


(provide add-streams
         cons-stream
         display-stream
         integers
         integers-starting-from
         mul-streams
         ones
         scale-stream
         show-stream
         stream-car
         stream-cdr
         stream-enumerate-interval
         stream-filter
         stream-for-each
         stream-map
         stream-null?
         stream-ref
         the-empty-stream)

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream null)

(define (stream-null? s) (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (void)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))



(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (show-stream s n)
  (for ([i (in-range n)]) 
    (printf "~n~a" (stream-ref s i))))


