#lang sicp

; (define (has-cycle x)
;   (define seen nil)
;   (define (iter cur)
;     (cond [(null? cur) #f]
;           [(memq (car cur) seen) #t]
;           [else (set! seen (cons (car cur) seen))
;                 (iter (cdr cur))]))
;   (iter x)
; )

(define (cycle? x) 
  (define visited nil) 
  (define (iter x) 
    (set! visited (cons x visited))
    (display visited)
    (newline) 
    (cond ((null? (cdr x)) false) 
          ((memq (cdr x) visited) true) 
          (else (iter (cdr x))))) 
  (iter x)) 

(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cddr l1) l1)
(cycle? l1)
;; #t
(cycle? l2)
;; #f