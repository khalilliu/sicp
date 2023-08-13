#lang racket/base

(require rackunit
       "4.20a.rkt")

(define Y-combinator-tests
    (test-suite
      "Tests for the meta-circular evaluator - The Y Combinator"
      (test-case 
        "Y Combinator - Factorial"
        (test-equal? 
          "4.21a: Y-Combinator factorial 10"
          (interpret '((lambda (n)
         ((lambda (fact) 
                  (fact fact n))
          (lambda (ft k)
            (if (= k 1)
                1
                (* k (ft ft (- k 1))))))) 10))  
          3628800)
       (test-equal? "ex4.21a: Y-Combinator factorial 15"
                 (interpret '((lambda (n)
                                ((lambda (fact)
                                   (fact fact n))
                                 (lambda (ft k)
                                   (if (= k 1)
                                       1
                                       (* k (ft ft (- k 1)))))))
                              15))
                 1307674368000)      
      )

      (test-case
        "Y Combinator - Fibonacci"
        (test-equal?
          "4.21b: Y-Combinator fibonacci 15"
          ((lambda (n)
  ((lambda (fib) (fib fib n)) ;; main fn
   (lambda (ft k)
      (if (< k 2) 
          k
          (+ (ft ft (- k 1))
             (ft ft (- k 2)))  );; recursive call
   )
  )) 15)
          610
        )
      ) 

      (test-case 
        "Y Combinator - even?"
        (interpret '(define (f x) ((lambda (x)
          ((lambda (even? odd?) (even? even? odd? x))
            (lambda (ev? od? k)
              (if (= k 0) true
                          (od? ev? od? (- k 1))))
            (lambda (ev? od? k)
              (if (= k 0) false
                          (ev? ev? od? (- k 1))))
        )) x)))
        (test-false "ex4.21b: Y-Combinator even - odd"
          (interpret '(f 17))
        )
        (test-true "ex4.21b: Y-Combinator even - odd"
          (interpret '(f 16))
        )
      )                
))



(require rackunit/text-ui)
(run-tests Y-combinator-tests 'normal)
