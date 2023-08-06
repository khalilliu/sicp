#lang racket

;; Ex 3.26
;;
(require scheme/mpair)
(provide make-table)

(define (mcaar xs) (mcar (mcar xs)))
(define (mcadr xs) (mcar (mcdr xs)))
(define (mcdar xs) (mcdr (mcar xs)))
(define (mcddr xs) (mcdr (mcdr xs))) 
(define (mcaddr xs) (mcar (mcdr (mcdr xs))))

(define (make-table)
  (define entry mcar)
  (define left-branch mcadr)
  (define right-branch mcaddr)
  (define make-record mcons)
  (define key-record mcar)
  (define value-record mcdr)
  (define (make-tree entry left right) (mlist entry left right))
  
  (define key=? equal?)
  (define (key<? key1 key2)
    (cond ((and (string? key1)
                (string? key2)) (string<? key1 key2))
          ((and (number? key1)
                (number? key2)) (< key1 key2))
          ((and (char? key1)
                (char? key2)) (char<? key1 key2))
          (else (error "Unsupported key types -- KEY<?" key1 key2))))
  (define (key>? key1 key2)
    (cond ((and (string? key1)
                (string? key2)) (string>? key1 key2))
          ((and (number? key1)
                (number? key2)) (> key1 key2))
          ((and (char? key1)
                (char? key2)) (char>? key1 key2))
          (else (error "Unsupported key types -- KEY>?" key1 key2))))
  
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((key=? (key-record x) (key-record (entry set))) true)
          ((key<? (key-record x) (key-record (entry set)))
           (element-of-set? x (left-branch set)))
          ((key<? (key-record x) (key-record (entry set)))
           (element-of-set? x (right-branch set)))))
  
  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x null null))
          ((key=? (key-record x) (key-record (entry set))) set)
          ((key<? (key-record x) (key-record (entry set)))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((key>? (key-record x) (key-record (entry set)))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))
  (define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
          (else (let* ((record (entry set-of-records))
                       (key-entry (key-record record)))
                  (cond ((key=? given-key key-entry) (value-record record))
                        ((key<? given-key key-entry) (lookup given-key 
                                                             (left-branch set-of-records)))
                        ((key>? given-key key-entry) (lookup given-key 
                                                             (right-branch set-of-records))))))))
  
  (define local-table null)
  (define (insert! key value)
    (set! local-table (adjoin-set (mcons key value) local-table)))
  
  (define (dispatch m)
    (cond ((eq? m 'lookup) (lambda (key) (lookup key local-table)))
          ((eq? m 'insert!) insert!)
          ((eq? m 'print) local-table)
          (else (error "Unknown operation -- TABLE" m))))
  dispatch)
