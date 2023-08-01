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
        [(contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree)))]
        [else (cons 1 (encode-symbol symbol (right-branch tree)))]))

(define (contains? symbol symbols)
  (cond [(null? symbols) #f]
        [(eq? symbol (car symbols)) #t]
        [else (contains? symbol (cdr symbols))]))



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

(define (display-pair pair)
  (if (null? pair)
      (newline)
      (and
       (newline)
       (display (car pair))
       (newline)
       (display "----------")
       (newline)
       (display (cdr pair))
       (newline))))

(define (display-block . items)
  (cond [(null? (car items)) (newline)]
        [else (and (display-pair (car items)) (display-block (cdr items)))]))


;; Test

(define rock-lyrics 
  '((A 2) 
    (BOOM 1) 
    (GET 2) 
    (JOB 2) 
    (NA 16) 
    (SHA 3) 
    (YIP 9) 
    (WAH 1)))


(define rock-tree 
  (generate-huffman-tree rock-lyrics))

(define song 
  '(GET A JOB 
        SHA NA NA NA NA NA NA NA NA 
        GET A JOB 
        SHA NA NA NA NA NA NA NA NA 
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
        SHA BOOM))

;; convenience func I wrote for printing a series of labeled messages



(display-block (cons "huffman-encoding length" 
                     (length (encode song rock-tree))) 
               (cons "min fixed-length encoding length" 
                     (* (log (length rock-lyrics) 2) (length song)))) 
