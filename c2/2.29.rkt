#lang sicp

(define (make-mobile left right) 
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure) 
  (list length structure)) 

(define (branch-length branch) 
  (car branch))

(define (branch-structure branch) 
  (car (cdr branch)))

;; get total weights of a binary tree
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(not (pair? mobile)) mobile]
        [else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile))))]))


;; Test  
(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(total-weight a) ;; 6

(define (torgue branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (cond [(not (pair? mobile)) #t]
        [else
         (and (= (torgue (left-branch mobile)) (torgue (right-branch mobile)))
              (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile))))]))


;; Test

(define d (make-mobile (make-branch 10 a) (make-branch 12 5)))
;; d = ((10 ((2 3) (2 3))) (12 5))
(balanced? d)


