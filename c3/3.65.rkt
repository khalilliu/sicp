#lang racket

(require "lib/stream.rkt")


(define (partial-sum s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sum s))))
;
(define (sqrt-improve guess x)
  (define (average x y) (/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
;(display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sum (pi-summands 1)) 4))
;(display-stream pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (sqr (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;(display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;(show-stream (accelerated-sequence euler-transform
;                                   pi-stream)
;             11)
;;4.0
;;3.166666666666667
;;3.142105263157895
;;3.141599357319005
;;3.1415927140337785
;;3.1415926539752927
;;3.1415926535911765
;;3.141592653589778
;;3.1415926535897953
;;3.141592653589795
;;+nan.0
;;done
;

(define (stream-limit stream tolerance)
  (cond ((stream-null? stream) null)
        ((stream-null? (stream-cdr stream)) (stream-car stream))
        (else (let ((1st (stream-car stream))
                    (2nd (stream-car (stream-cdr stream))))
                (if (> tolerance (abs (- 1st 2nd)))
                    2nd
                    (stream-limit (stream-cdr stream) tolerance))))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;(show-stream (sqrt-stream 2) 10)
;(newline)
;(sqrt 2 0.1)
;(sqrt 2 0.01)
;(sqrt 2 0.001)
;(sqrt 2 0.0001)

(define (ln-summands n)
  (cons-stream (/ 1.0 n)  
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream 
  (partial-sum (ln-summands 1)))

; (show-stream ln-stream 10)
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349207


; (show-stream (euler-transform ln-stream) 10)
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539683
; 0.6930657506744464

(show-stream (accelerated-sequence euler-transform
                                  ln-stream)
            10)

; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445
; 0.6931471805599427
; 0.6931471805599454