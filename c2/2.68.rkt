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


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond [(= bit 0) (left-branch tree)]
        [(= bit 1) (right-branch tree)]
        [else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond [(not (contains? symbol (symbols tree)))
         (error "can not found symbol" symbol)]
        [(leaf? tree) nil]
        [(contains? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree)))]
        [else (cons 1 (encode-symbol symbol (right-branch tree)))]))

(define (contains? symbol symbols)
    (cond [(null? symbols) #f]
          [(eq? symbol (car symbols)) #t]
          [else (contains? symbol (cdr symbols))]))

;; Test

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ;; (A D A B B C A)

(encode '(A D A B B C A) sample-tree)
