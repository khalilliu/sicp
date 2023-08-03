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



(define acc (make-account-with-password 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'secret-password 'deposit) 50)

((acc 'wrong-password 'withdraw) 50)

((acc 'wrong-password 'deposit) 50)