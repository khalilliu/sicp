#lang sicp

(define (make-account-with-password-limit balance password)
  (define try-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
        
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (wrong-password x)
    (begin (set! try-count (+ try-count 1))
           (if (= try-count 7)
              (call-the-cops x)
              (wrong-message x))))

  (define (wrong-message x) 
     (display "Incorrect password")
    (newline))

  (define (call-the-cops x)
    (display "Account Blocked, Call the cops")
    (newline))

  (define (dispatch p m)
    (cond [(eq? try-count 7) call-the-cops]
          [(eq? m 'withdraw)
            (if (eq? p password) withdraw wrong-password)]
          [(eq? m 'deposit)
            (if (eq? p password) deposit wrong-password)]
          [else (error "Unknown request -- MAKE-ACCOUNT" m)]))
  dispatch
)



(define acc (make-account-with-password-limit 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 50)
((acc 'wrong-password 'withdraw) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)
((acc 'wrong-password 'deposit) 50)