#lang racket

(define (make-interval a b) (cons a b)) 
(define (upper-bound interval) (max (car interval) (cdr interval))) 
(define (lower-bound interval) (min (car interval) (cdr interval))) 
  
(define (print-interval name i) 
  (newline) 
  (display name) 
  (display ": [") 
  (display (lower-bound i)) 
  (display ",") 
  (display (upper-bound i)) 
  (display "]")) 
  
;; Old multiplication (given) 
(define (old-mul-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y)))) 
    (make-interval (min p1 p2 p3 p4) 
                   (max p1 p2 p3 p4)))) 


(define (mul-interval x y)
  (define (endpoint-sign i)
    (cond [(and (>= (upper-bound i) 0) (>= (lower-bound i) 0)) 1]
          [(and (< (upper-bound i) 0) (< (lower-bound i) 0)) -1]
          [else 0]
          ))
  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-up (upper-bound x)) 
        (x-lo (lower-bound x)) 
        (y-up (upper-bound y))
        (y-lo (lower-bound y))
        )
    (cond [(> es-x 0)
           (cond [(> es-y 0) 
                  (make-interval (* x-lo y-lo) (* x-up y-up))]
                 [(< es-y 0) 
                  (make-interval (* x-up y-lo) (* x-lo y-up))]
                 [else 
                  (make-interval (* x-up y-lo) (* x-up y-up))])
           ]
          [(< es-x 0)
           (cond [(> es-y 0) 
                  (make-interval (* x-lo y-up) (* x-up y-lo))] 
                 [(< es-y 0) 
                  (make-interval (* x-up y-up) (* x-lo y-lo))]
                 [else 
                  (make-interval (* x-lo y-up) (* x-lo y-lo))])
           ]
          [else ;; x spans 0
           (cond [(> es-y 0)
                  (make-interval (* x-lo y-up) (* x-up y-up))]
                 [(< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-lo))]
                 [else
                  (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                 (max (* x-lo y-lo) (* x-up y-up)))])
           ])
    )
  )


;; test
(define (eq-interval? a b) 
  (and (= (upper-bound a) (upper-bound b)) 
       (= (lower-bound a) (lower-bound b))))


(define (ensure-mult-works aH aL bH bL)
  (let ((a (make-interval aL aH))
        (b (make-interval bL bH)))
    (if (eq-interval? (old-mul-interval a b)
                      (mul-interval a b))
        #t
        (error "new mult returns different value!"
               a
               b
               (old-mul-interval a b)
               (mul-interval a b)))))

;; run-tests
(ensure-mult-works  +10 +10   +10 +10) 
(ensure-mult-works  +10 +10   +00 +10) 
(ensure-mult-works  +10 +10   +00 +00) 
(ensure-mult-works  +10 +10   +10 -10) 
(ensure-mult-works  +10 +10   -10 +00) 
(ensure-mult-works  +10 +10   -10 -10) 
  
(ensure-mult-works  +00 +10   +10 +10) 
(ensure-mult-works  +00 +10   +00 +10) 
(ensure-mult-works  +00 +10   +00 +00) 
(ensure-mult-works  +00 +10   +10 -10) 
(ensure-mult-works  +00 +10   -10 +00) 
(ensure-mult-works  +00 +10   -10 -10) 
  
(ensure-mult-works  +00 +00   +10 +10) 
(ensure-mult-works  +00 +00   +00 +10) 
(ensure-mult-works  +00 +00   +00 +00) 
(ensure-mult-works  +00 +00   +10 -10) 
(ensure-mult-works  +00 +00   -10 +00) 
(ensure-mult-works  +00 +00   -10 -10) 
  
(ensure-mult-works  +10 -10   +10 +10) 
(ensure-mult-works  +10 -10   +00 +10) 
(ensure-mult-works  +10 -10   +00 +00) 
(ensure-mult-works  +10 -10   +10 -10) 
(ensure-mult-works  +10 -10   -10 +00) 
(ensure-mult-works  +10 -10   -10 -10) 
  
(ensure-mult-works  -10 +00   +10 +10) 
(ensure-mult-works  -10 +00   +00 +10) 
(ensure-mult-works  -10 +00   +00 +00) 
(ensure-mult-works  -10 +00   +10 -10) 
(ensure-mult-works  -10 +00   -10 +00) 
(ensure-mult-works  -10 +00   -10 -10) 
  
(ensure-mult-works  -10 -10   +10 +10) 
(ensure-mult-works  -10 -10   +00 +10) 
(ensure-mult-works  -10 -10   +00 +00) 
(ensure-mult-works  -10 -10   +10 -10) 
(ensure-mult-works  -10 -10   -10 +00) 
(ensure-mult-works  -10 -10   -10 -10) 