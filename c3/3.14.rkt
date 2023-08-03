#lang racket

(require scheme/mpair)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define x (mlist 'a 'b 'c 'd))

(mlist->list (mystery x))
;; '(d c b a)