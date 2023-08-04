#lang sicp

(define (make-table)
  (let ((local-table (list '*table* '())))
    (define (assoc key records)
      (cond [(null? records) #f]
            [(equal? records '(())) #f]
            [(equal? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    
    (define (find suitable keys)
      (let ((record (assoc (car keys) (cddr suitable))))
        (if record
            (if (null? (cdr keys))
                (list nil record)
                (find record (cdr keys)))
            (list keys suitable))))

    (define (new-branch! table keys value)
      (define (recurse keys value)
        (if (null? (cdr keys))
            (cons (car keys) (list value))
            (cons (car keys) (list nil (recurse (cdr keys) value)))))
      (if (not (pair? keys))
          #f
          (set-cdr! (cdr table) (cons (recurse keys value) (cddr table)))))

    ; (define (display)
    ;   (draw local-table))

    (define (insert! keys value)
      (let ((find-result (find local-table keys)))
        (let ((subkeys (car find-result))
              (suitable (cadr find-result)))
          (if (null? subkeys)
              (set-car! (cdr suitable) value)
              (new-branch! suitable subkeys value))))
      'ok)

    (define (lookup keys)
      (let ((find-result (find local-table keys)))
        (let ((subkeys (car find-result))
              (suitable (cadr find-result)))
          (if (null? subkeys)
              (let ((value (cadr suitable)))
                (if (equal? value nil)
                    #f
                    value))
              #f))))

    (define (dispatch m)
      (cond [(eq? m 'lookup) lookup]
            [(eq? m 'insert!) insert!]
            [(eq? m 'draw) (display)]
            [else (error "Unknown operation -- MAKE-TABLE" m)]))
    
    dispatch))


;;; Test

(define t-t-t-t (make-table)) 
(define (table-insert! value . keys) 
  ((t-t-t-t 'insert!) keys value)) 
(define (table-delete! . keys) 
  ((t-t-t-t 'insert!) keys nil)) 
(define (table-lookup . keys) 
  ((t-t-t-t 'lookup) keys)) 
(define (table-draw) 
  (t-t-t-t 'draw)) 

(table-insert! 'a 1 1) 
(table-lookup 1 1)
(table-insert! 'b 1 1) 
(table-insert! 'b 2) 
(table-insert! 'c 3) 
(table-insert! 'c 2 3 4) 
(table-insert! 'd 2 3 5) 
(table-insert! 'e 2 3 6) 
(table-lookup 3 4) 
(table-lookup 2 3) 
(table-lookup 2 3 4) 
(table-lookup 1) 
(table-insert! 'x 1 1) 
(table-insert! 'y 2 3 4) 
(table-lookup 1 1) 
(table-lookup 2 3 4) 
(table-delete! 2 3 4) 
(table-lookup 2 3 4) 
;; (table-draw) 