#lang racket

(require "lib/wires.rkt"
          "3.29.rkt")

(provide half-adder
         full-adder)

(define (half-adder a b s c)
  (with-wires (d e)
              (or-gate a b d)
              (and-gate a b c)
              (inverter c e)
              (and-gate d e s)
              'ok))


(define (full-adder a b c-in sum c-out)
  (with-wires (s c1 c2)
              (half-adder b c-in s c1)
              (half-adder a s sum c2)
              (or-gate c1 c2 c-out)
              'ok))

(define (ripple-carry-adder as bs ss c-out)
  (define (last-bit? as) (null? (cdr as)))

  (define (ripple-adder-iter as bs carry result)
    (cond [(null? as) 'ok]
          [else (full-adder (car as)
                            (car bs)
                            carry 
                            (car ss)
                            (if (last-bit? as) c-out result))
                (ripple-adder-iter (cdr as)
                                   (cdr bs)
                                   (cdr ss)
                                   result
                                   (make-wire))]))  

  (if (and (= (length as) (length bs))
           (= (length bs) (length ss)))
      (let ((carry (make-wire))
            (sum (make-wire)))
        (ripple-adder-iter as bs ss carry sum))
      (error "Inputs length must be the same size -- RIPPLE-ADDER"
             (length as)
             (length bs)
             (length ss)))
)





;; ripple-adder-delay for n-bits is
;;   n . (full-adder-delay)
;;   = n . (2 . half-adder-delay + or-gate-delay)
;;   
;; half-adder-delay : 
;;   inverter-delay + max (and-gate-delay + inverter, or-gate-delay)
;;   
;; Using the digital circuit for or-gates from Exercise 3.29
;;   or-gate-delay = and-gate-delay + 2. inverter-delay
;;   
;; half-adder-delay = and-gate-delay + 3. inverter-delay
;;
;; So ripple-adder-delay:
;; = n (2 . (and-gate-delay + 3. inverter-delay) + or-gate-delay)
;; = n (2 . (and-gate-delay + 3. inverter-delay) + and-gate-delay + 2. inverter-delay)
;; = n (3 . and-gate-delay + 8 inverter-delay)
