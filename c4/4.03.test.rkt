#lang racket/base

(require rackunit
         "4.03.rkt")

(provide eval-tests)

; Test suite
(define eval-tests
  (test-suite
   "Tests for the meta-circular evaluator ex4.03.rkt" 
   
   (test-case 
    "self-eval"
    (test-equal? "self-eval integer" 5 (interpret 5))
    (test-equal? "self-eval bignum" 666666666222 (interpret 666666666222))
    (test-equal? "self-eval rational" (/ 3 7) (interpret (/ 3 7)))
    (test-equal? "self-eval string" "hey" (interpret "hey")))
   
   
   (test-case 
    "test-quote"
    (test-equal? "quoted number"  2                 (interpret '(quote 2)))
    (test-equal? "quoted symbol" 'hello             (interpret '(quote hello))) 
    (test-equal? "quoted list"   '(jay wizz 2 watt) (interpret '(quote (jay wizz 2 watt)))))
   
   (test-case 
    "test-if"
    (test-equal? "test-if 1" 1 (interpret '(if (= 4 5) false 1)))
    (test-equal? "test-if 2" 1 (interpret '(if (= 5 5) 1 false)))
    (test-true "test-if 3" (interpret '(if false false true)))
    (test-true "test-if 4" (interpret '(if 1 true false))))
   
   ; note: -cond- also tests how -begin- works
   (test-true "test-cond 1" (interpret '(cond (false false) (else true))))
   (test-true "test-cond 2" (interpret '(cond (true true) (else false))))
   
   (test-true "test-cond 3"
              (interpret 
               '(cond 
                  ((= 5 6) false)
                  ((= 4 5) false)
                  ((= 5 5) true)
                  (else false))))
   
   (test-true "test-cond 4"
              (interpret 
               '(cond 
                  ((= 5 6) false)
                  ((= 4 5) false)
                  ((= 51 5) false)
                  (else (= 1 1))))) 
   
   (test-case 
    "test-var"
    (interpret '(define joe 12))
    
    (test-true "var exists" 
               (= (interpret 'joe) 12))
    (test-true "var can be used in expression" 
               (= (interpret '(+ joe 2)) 14))
    (test-true "var unmutated" 
               (interpret '(= joe 12)))
    
    (interpret '(define dave 5))
    (test-true "multiple vars can be used in expression"
               (= (interpret '(+ joe dave)) 17))
    (test-true "multiple vars values maintained"
               (not (interpret '(= joe dave))))
    
    (interpret '(set! dave 10))
    (interpret '(set! joe (+ 10 dave)))
    (test-true "check mutation with set!"
               (= (interpret '(+ joe dave)) 30))
    )
   
   (test-case
    "set! mutates outer scope correctly"
    (interpret '(define t 0))
    (interpret '(define a 4))
    (interpret '(define b 3))
    
    (test-equal? "set! inside let mutates defined value" 
                 (interpret '(begin
                               (set! t (* (+ t 4)
                                          (- b t)))
                               t))
                 12)
    (test-equal? "after set! outer scope still correct" 
                 (interpret 't)
                 12))   
   
   (test-case "test-function"
              ; simple function definition and application
              (interpret 
               '(define (sum a b)
                  (+ a b)))
              (interpret
               '(define (average x y)
                  (/ (sum x y) 2)))
              
              (interpret '(define xx 10))
              (interpret '(define yy 20))
              (test-equal? "test use of sum procedure" 6 (interpret '(sum 2 4)))
              (test-equal? "test use of average procedure" 15 (interpret '(average xx yy)))
              
              
              ; applying a lambda directly
              (test-equal? "test of direct lambda" 20
                           (interpret
                            '((lambda (x y) (+ x y)) 15 5)))
              
              ; define an explicit lambda
              (interpret
               '(define lsum 
                  (lambda (x y) (+ x y))))
              (test-equal? "test of lsum" 23 (interpret '(lsum 11 12)))
              (interpret
               '(set! lsum 
                      (lambda (x y) (- x y))))
              (test-equal? "test of redefined lsum" -1 (interpret '(lsum 11 12)))
              
              ; recursive function
              (interpret
               '(define (rsum x y)
                  (if (= y 0) 
                      x
                      (rsum (+ x 1) (- y 1)))))
              (test-equal? "test of rsum" 11 (interpret '(rsum 5 6)))
              (test-equal? "test of rsum" 6 (interpret '(rsum 0 6)))
              (test-equal? "test of rsum" 6 (interpret '(rsum 6 0)))
              
              ; returning a function from another function
              (interpret
               '(define (make-adder-func x)
                  (lambda (y) (+ x y))))
              (interpret
               '(define add2 (make-adder-func 2)))
              (test-equal? "test using make-adder" 12 (interpret '(add2 xx)))
              (test-equal? "test calling make-adder" 14 (interpret '((make-adder-func 4) 10)))
              
              ; accepting a function as an argument
              (interpret
               '(define (apply-twice func val)
                  (func (func val))))
              (test-equal? "named functional argument" 104 (interpret '(apply-twice add2 100)))
              (test-equal? "anonymous functional argument" 10000
                           (interpret 
                            '(apply-twice (lambda (x) (* x x)) 10)))
              
              ; complex high-order wizardry. -compose- takes two
              ; functions, and returns a function that is their
              ; composition
              ;
              (interpret
               '(define (compose f g) (lambda (x) (f (g x)))))
              (interpret '(define (square x) (* x x)))
              (interpret '(define (inc x) (+ x 1)))
              (test-equal? "higher order procedures" 121 (interpret '((compose square inc) 10)))
              (test-equal? "higher order procedures" 101 (interpret '((compose inc square) 10))))
   
   ))

(require rackunit/text-ui)
(run-tests eval-tests 'normal)
