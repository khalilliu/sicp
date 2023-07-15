#lang sicp

(define (pascal row col)
  (cond ((> col row) (error "unvalid col value"))
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(pascal 4 0)
(pascal 4 2)