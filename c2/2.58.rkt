#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define *precedence-table* 
  '( (maxop . 10000) 
     (minop . -10000) 
     (+ . 0) 
     (* . 1) ))

(define (operator? x)
  (define (iter op-pair)
    (cond [(null? op-pair) #f]
          [(eq? x (caar op-pair)) #t]
          [else (iter (cdr op-pair))]))
  (iter *precedence-table*))

(define (min-precedence a b)
  (if (precedence<? a b)
      a
      b))

(define (precedence<? a b)
  (< (precedence a) (precedence b)))

(define (precedence op)
  (define (iter op-pair)
    (cond [(null? op-pair)
           (error "Operator not defined -- PRECEDENCE:" op)]
          [(eq? op (caar op-pair))
           (cdar op-pair)]
          [else (iter (cdr op-pair))]))
  (iter *precedence-table*))

(define variable? symbol?)

(define (singleton? lst)
  (and (pair? lst) (= (length lst) 1)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list m1 '* m2))))

(define (=number? exp num) 
  (and (number? exp) (= exp num)))

(define (sum? expr)
  (eq? '+ (smallest-op expr)))

(define (prefix sym lst)
  (if (or (null? lst) (eq? sym (car lst)))
      nil
      (cons (car lst) (prefix sym (cdr lst)))))

(define (addend s)
  (let ((a (prefix '+ s)))
    (if (singleton? a)
        (car a)
        a))) 
  
(define (augend s)
  (let ((a (cdr (memq '+ s))))
    (if (singleton? a)
        (car a)
        a)))

(define (product? expr) 
  (eq? '* (smallest-op expr))) 

(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? a)
                    (min-precedence a b)
                    b))
              'maxop
              expr))

(define (multiplier p)
  (let ((m (prefix '* p)))
    (if (singleton? m)
        (car m)
        m))) 
  
(define (multiplicand p)
  (let ((m (cdr (memq '* p))))
    (if (singleton? m)
        (car m)
        m)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [else
         (error "unknown expression type -- DERIV" exp)]))


;; Test

; (smallest-op '(x + y * 3))
(deriv '(x + 3 * (x + y + 2)) 'x)

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y * (x + 3)) 'x)
;Value 88: ((x * y) + (y * (x + 3)))

;; Will extraneous parens throw our deriv for a loop?
(deriv '((x * y) * (x + 3)) 'x)
;Value 89: ((x * y) + (y * (x + 3)))

(deriv '(x * (y * (x + 3))) 'x)
;Value 90: ((x * y) + (y * (x + 3)))