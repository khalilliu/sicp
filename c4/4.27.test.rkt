#lang racket/base
(require rackunit
         "lib/lazy-eval.rkt")

;; It's difficult to import the tests for the strict evaluator and apply them to the 
;; lazy evaluator - all the relevant tests are repeated here.
;; The lazy evaluator doesn't include while, and unless

; Test suites
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

(define boolean-tests
  (test-suite 
   "Tests for the meta-circular evaluator ex4.04.rkt - and and or" 
   
   (test-case "test-or-and"
              (test-false "test-or with empty expression list" (interpret '(or))) 
              (test-false "test-or with single false" (interpret '(or false)))
              (test-true "test-or with single true" (interpret '(or true)))
              (test-true "test-or num-list 1" (interpret '(or 1 2 3)))
              (test-equal? "test-or list #f #f 3" 3 (interpret '(or false false 3)))
              (test-false "test-or #f #f" (interpret '(or false false)))
              (test-equal? "test-and 1 2 3" 3 (interpret '(and 1 2 3)))
              (test-false "test-and #f #f 3" (interpret '(and false false 3)))
              (test-false "test-and #f #f" (interpret '(and false false)))
              (test-true "test-and with empty expression list" (interpret '(and)))
              (test-false "test-and with single false" (interpret '(and false)))
              (test-true "test-and with single true" (interpret '(and true)))
              )))

(define special-cond-tests
  (test-suite 
   "Tests for the meta-circular evaluator ex4.05.rkt - cond with => clauses"
   
   (test-case 
    "cond special form : (<test> => <recipient>)"
    (test-equal? "cond t=>r with true value"
                 2
                 (interpret '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                                   (else false))))
    
    (test-equal? "cond t=>r without true value"
                 'fail
                 (interpret '(cond ((assoc 'z '((a 1) (b 2))) => cadr)
                                   (else (quote fail)))))
    
    (test-equal? "cond t=>r returning string value"
                 "something else"
                 (interpret '(cond ((assoc 'z '((a 1) (b 2))) => cadr)
                                   ("valueless" => (lambda (v) (if (equal? v "values")
                                                                   "really values"
                                                                   "something else"))))))
    
    (test-equal? "cond t=>r returning string value"
                 "really values"
                 (interpret '(cond ((assoc 'z '((a 1) (b 2))) => cadr)
                                   ("values" => (lambda (v) (if (equal? v "values")
                                                                "really values"
                                                                "something else")))))))
   ))

(define let-tests
  (test-suite 
   "Tests for the meta-circular evaluator ex4.06.rkt - let"
   (test-case 
    "test let"
    (test-equal?
     "simple let"
     6
     (interpret '(let ((a 1) (b 2) (c 3)) 
                   (+ a b c))))
    
    (interpret '(define (square x) (* x x)))
    (interpret '(define (square-1 x) (+ (square x)
                                        (square (- x 1)))))
    (test-equal? 
     "complex let with procedure calls"
     453220
     (interpret '(let ((a (square-1 7))
                       (b (+ (* 17 (square-1 13)) 11)))
                   (* a b)))))
   ))

(define let*-tests
  
  (test-suite
   "Tests for the meta-circular evaluator ex4.07.rkt - let*"
   (test-case
    "test let*"
    (test-equal? 
     "let* with 3 variabes"
     39
     (interpret  
      '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
         (* x z))))
    (test-equal? 
     "let* with a sequence of expression in the body"
     -8
     (interpret '(let* ((x 3)
                        (y (+ x 2))
                        (z (+ x y 5)))
                   (* x z)
                   (+ x y z)
                   (* (- y z))))))
   ))

(define named-let-tests
  
  (test-suite
   "Tests for the meta-circular evaluator ex4.08.rkt - named-let"
   
   (test-case 
    "named let"
    (interpret '(define (fib n)
                        (let fib-iter ((a 1) (b 0) (count n)) 
                          (if (= count 0) 
                              b
                              (fib-iter (+ a b) a (- count 1))))))
    (test-equal? "named let 1"
                 13 
                 (interpret '(fib 7)))
    (test-equal? "named let 2"
                 21 
                 (interpret '(fib 8)))
    (test-equal? 
     "recursive name-let to reverse a list"
     '(k j i h g f e d c b a)
     (interpret '(let rev-list ((xs '(a b c d e f g h i j k))
                                      (rev '()))
                         (cond ((null? xs) rev)
                               (else (rev-list (cdr xs)
                                               (cons (car xs)
                                                     rev))))))))
   ))

(define internal-definition-tests
  (test-suite
   "Tests for the meta-circular evaluator ex4.16c.rkt - internal definitions via transformation"
  
   (test-case 
    "internal definitions with recursion"
    (interpret '(define (f x)
                  (define (not-even? n)
                    (if (= n 0)
                        false
                        (not-odd? (- n 1))))
                  (define (not-odd? n)
                    (if (= n 0)
                        true
                        (not-even? (- n 1))))
                  (if (> x 2)
                      (not-even? x)
                      true)))
    (test-false "simple recursive internal definition f(8) => false"
                (interpret '(f 8)))
    (test-true "simple recursive internal definition f(2) => true"
               (interpret '(f 2)))
    (test-true "simple recursive internal definition f(3) => true"
               (interpret '(f 3)))
    
    (test-equal? "stateful recursive internal definition"
                 (interpret '((lambda (x y)
                                (define (u a) (cond ((= a 0) 0)
                                                    (else (v (- a 1)))))
                                (define (v a) (cond ((= a 0) 0)
                                                    (else (u (- a 1)))))
                                (u (+ x y)))
                              3 7))
                 0))
   (test-case 
    "self-eval"
    (test-equal? "self-eval integer" 5 (interpret 5))
    (test-equal? "self-eval bignum" 666666666222 (interpret 666666666222))
    (test-equal? "self-eval rational" (/ 3 7) (interpret (/ 3 7)))
    (test-equal? "self-eval string" "hey" (interpret "hey")))
   ))

(define letrec-tests
  (test-suite
   "Tests for the meta-circular evaluator ex4.20a.rkt - letrec"
   
   (test-case 
    "test-quote"
    (test-equal? "quoted number"  2                 (interpret '(quote 2)))
    (test-equal? "quoted symbol" 'hello             (interpret '(quote hello))) 
    (test-equal? "quoted list"   '(jay wizz 2 watt) (interpret '(quote (jay wizz 2 watt))))) 
   
   (test-case 
    "Unassigned"
    (test-equal? "basic quote"
                 (interpret '(quote *unassigned*))
                 '*unassigned*))
   (test-case 
    "Mutual recursion and simultaneous definition with letrec"
    (test-equal? "Hofstadter Female and Male sequence"
                 ; http://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences
                 (interpret '(letrec ((F (lambda (n)
                                           (if (= n 0) 1
                                               (- n (M (F (- n 1)))))))
                                      (M (lambda (n)
                                           (if (= n 0) 0
                                               (- n (F (M (- n 1))))))))
                               (F 19)))
                 12)
    (interpret '(define (is-even? x)
                  (letrec ((even?
                            (lambda (n)
                              (if (= n 0)
                                  true
                                  (odd? (- n 1)))))
                           (odd?
                            (lambda (n)
                              (if (= n 0)
                                  false
                                  (even? (- n 1)))))))
                  (even? x)))
    (test-false "Text if 7 is even - mutually recursive definition using letrec"
                (interpret '(is-even? 7)))
    (test-true "Text if 100 is even - mutually recursive definition using letrec"
               (interpret '(is-even? 100)))
    (test-equal? "factorial can call itself from within a letrec"
                 (interpret '(letrec ((fact
                                       (lambda (n)
                                         (if (= n 1)
                                             1
                                             (* n (fact (- n 1)))))))
                               (fact 10)))
                 3628800))
   ))

(define lazy-eval-tests
  (test-suite
   "Tests for the lazy evaluator" 
   
   (test-case 
    "Lazy evaluator"
    (interpret '(define (try a b)
                  (if (= a 0) 1 b)))
    (test-equal? "Lazy divide by zero"
                 (interpret '(try 0 (/ 1 0)))
                 1)
    
    (interpret '(define count 0))
    (interpret '(define (id x)
                  (set! count (+ count 1))
                  x))
    (interpret '(define w (id (id 10))))
    (test-equal? "Lazy nested mutator is delayed"
                 (interpret 'count)
                 1)
    (test-equal? "Lazy nested mutator evaluating w forces count"
                 (interpret 'w)
                 10)
    (test-equal? "Lazy nested mutator is delayed"
                 (interpret 'count)
                 2))
   )) 

(require rackunit/text-ui)
(for-each run-tests (list eval-tests
                          boolean-tests
                          special-cond-tests
                          let-tests
                          let*-tests
                          named-let-tests
                          internal-definition-tests
                          letrec-tests
                          lazy-eval-tests))

