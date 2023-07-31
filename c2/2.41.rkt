#lang sicp

(define (fold-right op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (fold-right op initial (cdr sequence)))))

(define (filter should-filter? sequence)
  (fold-right (lambda (x y)
                (if (should-filter? x)
                    (cons x  y)
                    y)) nil sequence))

(define (flatmap op sequence)
  (fold-right append nil (map op sequence)))

(define (range lo hi)
  (if (> lo hi)
      nil
      (cons lo (range (+ lo 1) hi))))


;;; solutions

(define (unique-tuple size max-num)
  (if (zero? size)
      (list nil)
      (flatmap (lambda (i)
                 (map (lambda (j) (append j (list i)))
                      (unique-tuple (- size 1) (- i 1))))
               (range 1 max-num))))


(define (triples-of-sum n k)
 (filter (lambda (seq) (eq? (fold-right + 0 seq) k))
         (unique-tuple 3 n)))

;; Test

(unique-tuple 3 5)
; ((1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 5) (1 3 5) (2 3 5) (1 4 5) (2 4 5) (3 4 5))

(triples-of-sum 6 11)
; ((2 3 5) (1 4 5))


