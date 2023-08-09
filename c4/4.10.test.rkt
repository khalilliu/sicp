#lang racket/base
(require rackunit
         "4.10.rkt")

; Test suite

(define new-keyword-tests
  (test-suite
    "Tests for the meta-circular evaluator ex4.10.rkt - new keywords"
    
    (test-case
      "set! mutates outer scope correctly"
      (interpret '(fun t 0))
      (test-equal? "set! inside let mutates defined value" 
                 (interpret '(let ((a 4) (b 3))
                               (set! t (* (+ t 4)
                                          (- b t)))
                               t))
                 12)
      (test-equal? "after set! outer scope still correct" 
                 (interpret 't)
                 12)
    )

    (test-case
      "test while"
      (interpret '(fun xx 5))
      (interpret '(fun yy 6))
      (interpret '(while (> xx 0)
                       (set! xx (- xx 1))
                       (set! yy (+ yy 1))))
    (test-equal? "post while condition xx"
                 (interpret 'xx)
                 0)
    (test-equal? "post while condition yy"
                 (interpret 'yy)
                 11)
    (interpret '(while false
                       (set! xx (- xx 1))))
    (test-equal? "post empty while loop"
                 (interpret 'xx)
                 0)
    )
    
    (test-case 
      "test-or-and"
      (test-true "test-or num-list 1" (interpret '(|| 1 2 3)))
      (test-equal? "test-or list #f #f 3" 
                 (interpret '(|| false false 3)) 
                 3)
      (test-false "test-or #f #f" 
                  (interpret '(|| false false)))
      (test-equal? "test-and 1 2 3" 
                  (interpret '(&& 1 2 3))
                  3) 
      (test-false "test-and #f #f 3" 
                  (interpret '(&& false false 3)))
      (test-false "test-and #f #f" 
                      (interpret '(&& false false)))
    )
  )  
)


(require rackunit/text-ui)
(run-tests new-keyword-tests 'normal)
