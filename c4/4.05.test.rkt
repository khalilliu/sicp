#lang racket/base

(require rackunit
         "4.05.rkt")

;; Test suite
(define cond=>-tests
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
                                                            "something else"))))))
  )))


(require rackunit/text-ui)
(run-tests cond=>-tests 'normal)