#lang racket

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt b) dispatch))
        (set-value! b (* (get-value a) (get-value a)) dispatch)))
  (define (process-forget-value)
    (forget-value! a dispatch)
    (forget-value! b dispatch)
    (process-new-value))
  (define (dispatch requrest)
    (cond [(eq? requrest 'I-have-a-value) 
            (process-new-value)]
          [(eq? requrest 'I-lost-my-value)
            (process-forget-value)]
          [else 
            (error "Unknown request -- SQUARER" request)]))
  (connect a dispatch)
  (connect b dispatch)
  dispatch)

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(set-value! A 13 'user)
(get-value B)
