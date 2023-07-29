#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))



(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y-k k) (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2)) (y-k k)))
  
  (/ (* h (sum term 0 inc n)) 3))


(simpsons-rule cube 0 1 100)

(simpsons-rule cube 0 1 1000)
                        