#lang sicp


(define items (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr items)))))
;; => 7

(car (car (list (list 7))))
;; => 7


(define a (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; a =>  (1 (2 (3 (4 (5 (6 7))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr a))))))))))))

