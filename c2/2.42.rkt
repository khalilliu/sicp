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


;; exercise

(define empty-board '())

(define (adjoin-position row col rest-of-queens)
  (cons (list row col) rest-of-queens))

(define (safe? k positions) 
  (define (queen-not-safe? q1 q2) 
    (or (= (car q1) (car q2))
        (= (cadr q1) (cadr q2))
        (= (abs (- (car q1) (car q2))) 
           (abs (- (cadr q1) (cadr q2)))))) 
        
  (let ((p1 (car positions))
        (rest (cdr positions)))
    (zero? (fold-right
            (lambda (p2 acc)
              (if (queen-not-safe? p1 p2)
                  (+ acc 1)
                  acc))
            0
            rest))))

(define (queens board-size)
  (define (queen-cols k)
    (if (zero? k)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Test

(length (queens 8)) ;; 92
(length (queens 11)) ;; 2680
