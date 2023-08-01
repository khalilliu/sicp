#lang sicp

(define (equal? a b)
  (cond [(and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b)))]
        [(or (pair? a) (pair? b)) #f]
        [else (eq? a b)]))


(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (adjoin x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond [(and (null? set1) (null? set2)) nil]
        [(null? set1) set2]
        [(null? set2) set1]
        [(not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2))]
        [else (union-set (cdr set1) set2)]))


(union-set '(1 2 3 4) '(5 1 3))