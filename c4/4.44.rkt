#lang racket

(require "lib/amb.rkt")

;; Exercise 4.44

(define (range low high)
  (if (> low high) 
      '()
      (cons low (range (+ low 1) high))))

(define empty-board null)
(define adjoin-position cons)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (map (lambda (rest-of-queens)
                    (let ((new-board (adjoin-position 
                                      (an-element-of (range 1 board-size))
                                      rest-of-queens)))
                          (require (safe? new-board))
                          new-board))
             (queen-cols (- k 1)))))
  
  (queen-cols board-size))

(define (safe? position)
  (define board-size (length position))
  (define (safe-diagonal? position)
    (define (col-safe? new-row col position)
      (cond [(> col board-size) true]
            [(= (abs (- new-row (car position)))
                (abs (- col 1))) false]
            [else (col-safe? new-row (+ col 1) (cdr position))]))
    (col-safe? (car position) 2 (cdr position)))

  (define (safe-horizontal? position)
    (not (member (car position) (cdr position))))

  (or (= board-size 1)
      (and (safe-horizontal? position)
           (safe-diagonal? position))))


(queens 8)
(try-again)
(try-again)
(try-again)
;;; ((5 3 1 6 4 2))
;;; ((4 1 5 2 6 3))
;;; ((3 6 2 5 1 4))
;;; ((2 4 6 1 3 5))