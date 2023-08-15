#lang racket
(require compatibility/defmacro)

(define-macro (unless-lazy predicate action alternative) 
  `(if (not ,predicate) ,action ,alternative)) 

(define (unless-applicative predicate action alternative) 
  (if (not predicate) action alternative)) 

(define (factorial n) 
  (display "factorial called with " )
  (display n)
  (newline)
  (when (< n -10) 
          (error "factorial called with negative number"))
  (unless-lazy (= n 1) 
          (* n (factorial (- n 1))) 
          1)) 
        
(factorial 5) 