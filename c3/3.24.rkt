#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond [(null? records) #f]
            [(same-key? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    (define (lookup key-1 key-2)
      (let ((suitable (assoc key-1 (cdr local-table))))
        (if suitable
          (let ((record  (assoc key-2 (cdr suitable))))
            (if record 
                (cdr record)
                #f)))))
    (define (insert! key-1 key-2 value)
      (let ((suitable (assoc key-1 (cdr local-table))))
        (if suitable 
          (let ((record (assoc key-2 (cdr suitable))))
            (if record
                (set-cdr! record value)
                (set-cdr! suitable 
                          (cons (cons key-2 value)
                                (cdr local-table)))))
          (set-cdr! local-table 
                    (cons (list key-1 
                                (cons key-2 value))
                          (cdr local-table))))))
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unkown operation -- TABLE" m)]))
  dispatch))


 (define operation-table (make-table 
                          (lambda (x y) (< (abs (- x y)) 0.1)))) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc!)) 

; test 
(put 1.0 1.0 'hello) 
(get 1.01 1.01) 

