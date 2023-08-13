#lang racket

(require "4.20a.rkt")


((lambda (n)
         ((lambda (fact) 
                  (fact fact n))
          (lambda (ft k)
            (if (= k 1)
                1
                (* k (ft ft (- k 1))))))) 10)

;; 3628800

((lambda (n)
  ((lambda (fib) (fib fib n)) ;; main fn
   (lambda (ft k)
      (if (< k 2) 
          k
          (+ (ft ft (- k 1))
             (ft ft (- k 2)))  );; recursive call
   )
  )) 15)

;; 610


((lambda (n)
  ((lambda (even? odd?) (even? even? odd? n))
    (lambda (ev? od? k)
      (if (= k 0) true
                  (od? ev? od? (- k 1))))
    (lambda (ev? od? k)
      (if (= k 0) false
                  (ev? ev? od? (- k 1))))
  ))
14)