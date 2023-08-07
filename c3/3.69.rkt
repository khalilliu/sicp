#lang racket

(require "lib/stream.rkt")

(define (square x)
  (* x x))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define first-of-integer-pair 
  (stream-map car (pairs integers integers)))

; (define (triples s t u)
;   (let ((pairs-tu (pairs t u)))
;     (define (rec si i ptu top-i)
;       (cons-stream
;         (cons (stream-car si) (stream-car ptu))
;         (if (= i (stream-car top-i))
;             (rec s 1 (stream-cdr ptu) (stream-cdr top-i))
;             (rec (stream-cdr si) (+ i 1) ptu top-i))))
;     (rec s 1 pairs-tu first-of-integer-pair)))

(define (triples s t u)
        (cons-stream 
          (list 
            (stream-car s)
            (stream-car t) 
            (stream-car u))
          (interleave
            (stream-map 
              (lambda (x) 
                (cons (stream-car s) x))
              (stream-cdr (pairs t u)))
            (triples 
              (stream-cdr s)
              (stream-cdr t)
              (stream-cdr u)))))

(define triples-integers 
   (triples integers integers integers)) 

(define (pythagorean? a b c) 
  (= (square c) 
    (+ (square a) (square b)))) 

(define pythagorean-triples 
  (stream-filter 
    (lambda (triple)
      (apply pythagorean? triple))
    triples-integers))

(stream-ref pythagorean-triples 0) ; (3 4 5) 
(stream-ref pythagorean-triples 1) ; (6 8 10) 
(stream-ref pythagorean-triples 2) ; (5 12 13) 
(stream-ref pythagorean-triples 3) ; (9 12 15) 
(stream-ref pythagorean-triples 4) ; (8 15 17) 