#lang sicp

(define (filter-accumulate combiner null-value filter? term a next b)
  (cond [(> a b) null-value]
        [(not (filter? (term a)))
         (filter-accumulate combiner null-value filter? term (next a) next b)]
        [else 
          (combiner (term a)
                (filter-accumulate combiner null-value filter? term (next a) next b))]))

(define (filter-accumulate2 combiner null-value filter? term a next b)
  (define (iter a result)
    (cond [(> a b) result]
          [(not (filter? (term a))) (iter (next a) result)]
          [else (iter (next a) (combiner result (term a)))]))
  (iter a null-value)
  )

(define (inc x) (+ x 1))

(define (add a b) (+ a b))

(define (product a b) (* a b))

(define (identify a) a)

(define (prime? a)
  (define (f n c)
    (cond [(< n 2) #f]
          [(< n (* c c)) #t]
          [(zero? (modulo n c)) #f]
          [else (f n (inc c))])
    )
  (f a 2)
  )

(define (example1 a b)
  (filter-accumulate2 add 0 prime? identify a inc b))

(define (example2 n)
  (define (filter? a)
    (cond [(eq? (gcd a n) 1) #t]
          [else #f]))
  (filter-accumulate2 product 1 filter? identify 1 inc n)
  )


(example1 1 10000)
; 10 (2 + 3 + 5)
(example2 10000) 
; 189 (3 * 7 * 9)