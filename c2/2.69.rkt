#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
       (adjoin-set ;; reorder node by weight
        (make-code-tree (car leaves) (cadr leaves)) ;; merge two node
        (cddr leaves)))))


;; Test

(generate-huffman-tree '((a 7) (b 8) (g 2) (t 6)))
;; (((leaf g 2) (leaf t 6) (g t) 8) ((leaf a 7) (leaf b 8) (a b) 15) (g t a b) 23)