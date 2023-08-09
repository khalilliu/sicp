#lang racket/base

(require rackunit
         "4.13.rkt")

(define unbinding-tests
  (test-suite
   "Tests for the meta-circular evaluator ex4.13.rkt - make-unbound!"
   
   (test-case
    "Unbinding variables"
    (interpret '(define v1 0))
    (interpret '(make-unbound! v1))
    (test-exn "make-unbound removes binding" 
              exn:fail?
              (lambda ()
                (interpret 'v1)))

    (test-exn "make-unbound cannot remove outer binding" 
              exn:fail?
              (lambda ()
                (interpret '(define v2 0))
                (interpret '(define (inner-binding)
                              (make-unbound! v2)))
                (interpret '((inner-binding)))))
    (interpret '(define v3 0))
    (interpret '(define (inner-binding-2)
                  (define v3 5)
                  (make-unbound! v3)
                  v3))
    (test-equal? "make-unbound removes inner binding; outer binding still accessible" 
              (interpret '(inner-binding-2))
              0))))


(require rackunit/text-ui)

(run-tests unbinding-tests 'normal)

