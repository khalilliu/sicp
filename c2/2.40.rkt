#lang sicp

(define (fold-right op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (fold-right op initial (cdr sequence))))) 

(define (prime? x) 
  (define (test divisor) 
    (cond ((> (* divisor divisor) x) true) 
          ((= 0 (remainder x divisor)) false) 
          (else (test (+ divisor 1))))) 
  (test 2))

(define (filter should-filter? sequence)
  (fold-right (lambda (x y)
                (if (should-filter? x)
                    (cons x  y)
                    y)) nil sequence))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (flatmap op sequence)
  (fold-right append nil (map op sequence)))


(define (enumerate-interval lo hi)
  (if (> lo hi)
      nil
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Test
(unique-pairs 5) 
;; ((1 2) (1 3) (2 3) (1 4) (2 4) (3 4) (1 5) (2 5) (3 5) (4 5))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; Test: 
(prime-sum-pairs 6) 
;; => ((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7) (1 6 7) (5 6 11)) 
  
(define (permination sequence)
  (if (null? sequence)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permination (remove x sequence))))
               sequence)))


;; Test

(permination '(1 2 3))
; ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))