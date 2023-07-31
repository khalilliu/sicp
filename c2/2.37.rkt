#lang sicp

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;; Test

(define my-matrix (list (list 1 2 3 4) 
                        (list 4 5 6 6) 
                        (list 6 7 8 9))) 

(dot-product (car my-matrix) 
             (car (cdr my-matrix))) ; 56

(matrix-*-vector my-matrix (car my-matrix)) ; (30 56 80)

(transpose my-matrix) ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9)) 

(matrix-*-matrix my-matrix (transpose my-matrix)) ; ((30 56 80) (56 113 161) (80 161 230)) 
