#lang sicp

(define (make-account-with-password balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
        
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (wrong-password x)
    (display "Incorrect password")
    (newline))

  (define (dispatch p m)
    (cond [(eq? m 'withdraw)
            (if (eq? p password) withdraw wrong-password)]
          [(eq? m 'deposit)
            (if (eq? p password) deposit wrong-password)]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))
  dispatch
)

(define (make-joint acc password joint-password)
  (define (dispatch key m)
    (cond ((not (eq? key joint-password))
           (error "Incorrect password -- MAKE-JOINT") )
          ((eq? m 'withdraw) (acc password 'withdraw))
          ((eq? m 'deposit)  (acc password 'deposit))
          (else (error "Unknown request -- MAKE-JOINT" m))))
  dispatch)


(define peter-acc (make-account-with-password 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
 
((peter-acc 'open-sesame 'withdraw) 10)
;; -> 90
 
((paul-acc 'rosebud 'withdraw) 10)
;; -> 80
((paul-acc 'open-sasame 'withdraw) 10)
;; -> ERROR: Incorrect password -- MAKE-JOINT
