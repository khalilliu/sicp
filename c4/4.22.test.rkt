#lang racket/base
(require rackunit
         "4.22.rkt")


;; Test suite
(define eval-tests
  (test-suite
    "Tests for the meta-circular analysing evaluator ex4.22.rkt" 

    (test-case 
      "self-eval"
      (test-equal? "self-eval integer" 5 (interpret 5))
      (test-equal? "self-eval bignum" 666666666222 (interpret 666666666222))
      (test-equal? "self-eval rational" (/ 3 7) (interpret (/ 3 7)))
      (test-equal? "self-eval string" "hey" (interpret "hey"))
      )
    
    (test-case
      "test-quote"
      (test-equal? "quoted number"  2   (interpret '(quote 2)))
      (test-equal? "quoted symbol" 'hello (interpret '(quote hello))) 
      (test-equal? "quoted list"   '(jay wizz 2 watt) (interpret '(quote (jay wizz 2 watt))))
    )

    (test-case 
      "test-if"
      (test-equal? "test-if 1" 1 (interpret '(if (= 4 5) false 1)))
      (test-equal? "test-if 2" 1 (interpret '(if (= 5 5) 1 false)))
      (test-true "test-if 3" (interpret '(if false false true)))
      (test-true "test-if 4" (interpret '(if 1 true false)))
    )

    ;;  note: -cond- also tests how -begin- works
    (test-true "test-cond 1" (interpret '(cond (false false) (else true))))
    (test-false "test-cond 2" (interpret '(cond (true true) (else false))))

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
      "cond special form : (<test> => <recipient>)"
      (test-equal?  "cond t=>r with true value"
                    2 
                    (interpret '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                                      (else false)))
      )

      (test-equal?  "cond t=>r without true value"
                    'fail
                    (interpret '(cond ((assoc 'z '((a 1) (b 2))) => cadr)
                                      (else (quote fail))))
      )

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

    (test-case 
      "test-var"
      (interpret '(define joe 12))
      (test-true "var exists" (= (interpret 'joe) 12))
      (test-true "var can be used in expression" (= (interpret '(+ joe 2)) 14))
      (test-true "var unmutated" (interpret '(= joe 12)))

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
        "test-function"
        (interpret '(define (sum a b) (+ a b)))
        (interpret '(define (average a b) (/ (sum a b) 2)))

        (interpret '(define xx 10))
        (interpret '(define yy 20))
        (test-equal? "test use of sum procedure" 
                      6 
                     (interpret '(sum 2 4)))
        (test-equal? "test use of average procedure" 
                      15 
                     (interpret '(average xx yy)))

        ; applying a lambda directly
        (test-equal? "test of direct lambda" 
                     20
                     (interpret '((lambda (x y) (+ x y)) 10 10)))

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
        (interpret '(define (compose f g) (lambda (x) (f (g x)))))
        (interpret '(define (square x) (* x x)))
        (interpret '(define (inc x) (+ x 1)))
        (test-equal? "higher order procedures" 121 (interpret '((compose square inc) 10)))
        (test-equal? "higher order procedures" 101 (interpret '((compose inc square) 10)))
      )

      (test-case
        "test-let"
        (test-equal? 
          "simple-let"
          6
          (interpret '(let ((x 2) (y 3)) (+ x y))))
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


(require rackunit/text-ui)
(run-tests eval-tests 'normal)
