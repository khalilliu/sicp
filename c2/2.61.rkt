#lang sicp

(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set))))) 
  
(define (adjoin-set x set) 
  (cond [(null? set) (list x)]
        [(< x (car set)) (cons x set)]
        [(= x (car set)) set]
        [else (cons (car set) (adjoin-set x (cdr set)))])) 
  
(define (union-set set1 set2) 
  (cond [(and (null? set1) (null? set2)) nil]
        [(null? set1) set2]
        [(null? set2) set1]
        [else
         (let ((s1 (car set1))
               (s2 (car set2))
               (r1 (cdr set1))
               (r2 (cdr set2)))
           (cond [(= s1 s2) (cons s1 (union-set r1 r2))]
                 [(> s1 s2) (cons s2 (union-set set1 r2))]
                 [else (cons s1 (union-set r1 set2))]))]))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) nil]
        [else
         (let ((s1 (car set1))
               (s2 (car set2))
               (r1 (cdr set1))
               (r2 (cdr set2)))
           (cond [(= s1 s2) (cons s1 (intersection-set r1 r2))]
                 [(> s1 s2) (intersection-set set1 r2)]
                 [else (intersection-set r1 set2)]))]))

;; Test

(define s1 '(1 3 4 5 7 9))
(define s2 '(1 2 5 6 8 10))

(adjoin-set 10 s1)

(union-set s1 s2)

(intersection-set s1 s2)