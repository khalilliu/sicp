#lang sicp

(define (make-deque) (cons '() '()))

(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque) 
      (val-decell (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque) 
    (val-decell (rear-ptr deque))))

(define (set-first-deque! deque decell)
  (set-front-ptr! deque decell)
  (set-rear-ptr! deque decell))

(define (front-insert-deque! deque item)
  (let ((decell (make-decell '() item '())))
    (cond [(empty-deque? deque)
            (set-first-deque! deque decell)]
          [else (connect-decell! decell (front-ptr deque))
                (set-front-ptr! deque decell)])))

(define (rear-insert-deque! deque item)
  (let ((decell (make-decell '() item '())))
    (cond [(empty-deque? deque) 
            (set-first-deque! deque decell)]
          [else (connect-decell! (rear-ptr deque) decell)
                (set-rear-ptr! deque decell)])))

 (define (front-delete-deque! deque) 
   (cond ((empty-deque? deque) 
          (error "FRONT-DELETE called with an empty deque" deque)) 
         (else 
          (set-front-ptr! deque (right-decell (front-ptr deque))) 
          (if (not (empty-deque? deque)) 
              (set-left-decell! (front-ptr deque) '())) 
          deque))) 

(define (rear-delete-deque! deque) 
   (cond ((empty-deque? deque) 
          (error "REAR-DELETE called with an empty deque" deque)) 
         (else 
          (display (rear-ptr deque))
          (set-rear-ptr! deque (left-decell (rear-ptr deque))) 
          (if (not (empty-deque? deque)) 
              (set-right-decell! (rear-ptr deque) '())) 
          deque)))

(define (deque->list deque)
  (define (iter decell)
    (if (null? decell) 
        nil
        (cons (val-decell decell) (iter (right-decell decell)))))
  (if (empty-deque? deque)
      nil 
      (iter (front-ptr deque))))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque decell) (set-car! deque decell))
(define (set-rear-ptr! deque decell) (set-cdr! deque decell))

(define (make-decell left value right)
  (cons (cons value left) right))

(define (val-decell decell)
  (caar decell))

 (define (left-decell decell) 
   (if (not (null? (cdr (car decell)))) 
       ((cdr (car decell))) 
       '())) 

(define (right-decell decell)
  (cdr decell))

(define (set-right-decell! decell right)
  (set-cdr! decell right))

(define (set-left-decell! decell left)
  (set-cdr! (car decell) 
            (lambda () left)))

(define (connect-decell! l-decell r-decell)
  (set-left-decell! r-decell l-decell)
  (set-right-decell! l-decell r-decell))


;; Test
(define deq (make-deque)) 
(front-insert-deque! deq 'a) 
(front-insert-deque! deq 'b) 
(rear-insert-deque! deq 'z) 
(rear-insert-deque! deq 'y) 

(define (newline-display exp) 
  (newline) (display exp)) 

(newline-display (deque->list deq)) 
;;Value: (b a z y) 
(newline-display (front-deque deq)) 
;;Value: b 
(front-delete-deque! deq) 
(newline-display (front-deque deq)) 
;;Value: a 
(rear-delete-deque! deq) 
(newline-display (rear-deque deq)) 
;;Value: z 