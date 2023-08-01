#lang sicp

;; 2.65
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '())
  )

(define (list->tree lst)
  (car (partial-tree lst (length lst))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (union-set set1 set2)
  (define (union-list set1 set2) 
    (cond ((null? set1) set2) 
          ((null? set2) set1) 
          (else  (let ((x1 (car set1)) (x2 (car set2))) 
                   (cond ((equal? x1 x2) 
                          (cons x1 (union-list (cdr set1) (cdr set2)))) 
                         ((< x1 x2) 
                          (cons x1 (union-list (cdr set1) set2))) 
                         ((< x2 x1) 
                          (cons x2 (union-list set1 (cdr set2)))))))))
  (list->tree (union-list (tree->list set1) (tree->list set2))))

(define (intersection-set set1 set2) 
  (define (intersection-list set1 set2) 
    (cond ((null? set1) '()) 
          ((null? set2) '()) 
          (else (let ((x1 (car set1)) (x2 (car set2))) 
                  (cond ((equal? x1 x2) 
                         (cons x1 (intersection-list (cdr set1) (cdr set2)))) 
                        ((< x1 x2) 
                         (intersection-list (cdr set1) set2)) 
                        ((< x2 x1) 
                         (intersection-list set1 (cdr set2)))))))) 
  (list->tree (intersection-list (tree->list set1) (tree->list set2))))



;; Test
(union-set (list->tree '(3 5 10)) 
                  (list->tree '(1 2 3 4 5 7)))

;; (4 (2 (1 () ()) (3 () ())) (7 (5 () ()) (10 () ())))

(intersection-set (list->tree '(3 5 10)) 
                  (list->tree '(1 2 3 4 5 7)))

 ;; => (5 (3 () ()) ())