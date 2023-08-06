#lang racket

(require racket/mpair)

(define nil '())

; (provide make-queue)


; (define (mproxy f ls) 
;   (let ((res (f (mlist->list ls))))
;     (if (pair? res)
;         (list->mlist res)
;         res)))
; (define (mcaar xs) (mproxy caar xs))
; (define (mcadr xs) (mproxy cadr xs))
; (define (mcdar xs) (mproxy cdar xs))
; (define (mcddr xs) (mproxy cddr xs))

; (define (make-queue)
;   (let ((front-ptr nil)
;         (rear-ptr nil))
;     (define (set-front-ptr! item) (set! front-ptr item))
;     (define (set-rear-ptr! item)  (set! rear-ptr item))
;     (define (empty-queue?) (null? front-ptr))
;     (define (front-queue)
;       (if (empty-queue?)
;           (error "FRONT called with an empty queue")
;           (mcar front-ptr)))
;     (define (insert-queue! item)
;       (let ((new-pair (mcons item '())))
;         (cond ((empty-queue?)
;                (set-front-ptr! new-pair)
;                (set-rear-ptr! new-pair)
;                (mcons front-ptr rear-ptr))
;               (else
;                (set-mcdr! rear-ptr new-pair)
;                (set-rear-ptr! new-pair)
;                (mcons front-ptr rear-ptr))))) 
;     (define (delete-queue!)
;       (cond ((empty-queue?)
;              (error "DELETE! called with an empty queue"))
;             (else
;              (set-front-ptr! (mcdr front-ptr))
;              (mcons front-ptr rear-ptr)))) 
;     (define (print)
;       (cond ((empty-queue?)
;              (error "DELETE! called with an empty queue"))
;             (else
;              (set-front-ptr! (mcdr front-ptr))
;              (mcons front-ptr rear-ptr)))) 
;     (define (dispatch action)
;       (cond ((eq? action 'insert-queue!) insert-queue!)
;             ((eq? action 'delete-queue!) delete-queue!)
;             ((eq? action 'empty-queue?)  empty-queue?)
;             ((eq? action 'front-queue)   front-queue)
;             ((eq? action 'print)         front-ptr)
;             (else (error "Unknown action -- MAKE-QUEUE" action))))
;     dispatch))


(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (mcons nil nil))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an 
              empty queue" queue)
    (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-mcdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
          (error "DELETE! called with 
                an empty queue" queue)]
        [else (set-front-ptr! 
                queue
                (mcdr (front-ptr queue)))
              queue]))

(provide 
        nil
        make-queue
        empty-queue?
        front-queue
        delete-queue!
        insert-queue!)