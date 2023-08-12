#lang racket

(require "lib/base-eval.rkt"
          "4.04.rkt") ;; boolean expression

(provide interpret)

;; this version of the evaluator includes these expression types:
;;  self-evaluating 
;;  variable
;;  quote 
;;  define 
;;  set! 
;;  if
;;  lambda 
;;  begin 
;;  application? 
;;  and
;;  or
;;  cond 
;;  special cond <test> => <recipient>


; (define (eval-cond=> exp env)
;   (eval (cond=>->if exp) env))

; (define (cond=>? exp)                       (tagged-list? exp 'cond))
; (define (cond=>-clauses exp)                (mcdr  exp))
; (define (cond=>-else-clause? clause)        (eq? (cond=>-predicate clause) 'else))
; (define (cond=>-predicate clause)           (mcar clause))
; (define (cond=>-recipient clause)           (mcaddr clause))
; (define (cond=>-recipient-clause? clause)   (eq? (mcadr clause) '=>))

; ; this checks against the 2 forms for cond clauses
; ; 1) ((pred-clauses) (value-clauses)) -> result is (value-clauses)
; ; 2) ((pred-values) => proc)         -> result is (proc v)

; (define (make-cond=>-recipient clause predicate)
;   (mlist (cond=>-recipient clause) predicate)) ;; (proc pred)

; ;; 1) (proc pred)
; ;; 2) (value-clauses)
; (define (cond-consequent clause predicate)
;   (if (cond=>-recipient-clause? clause)
;       (make-cond=>-recipient clause predicate)
;       (sequence->exp (cond=>-actions clause))))

; ;; (value-clauses)
; (define (cond=>-actions clause)
;   (mcdr clause))

; (define (cond=>->if exp)
;   (expand=>-clauses (cond=>-clauses exp)))

; (define (expand=>-clauses clauses)
;   (if (null? clauses)
;       'false    ;; no else clause
;       (let ((first (mcar clauses))
;             (rest  (mcdr clauses)))
;         (if (cond=>-else-clause? first) ;; [else ...]
;             (if (null? rest)
;                 (sequence->exp (cond=>-actions first))
;                 (error "ELSE clause isn't last -- cond=>->if"
;                        clauses))
;             (let ((predicate (cond=>-predicate first)))
;               (make-if predicate
;                       (cond-consequent first predicate)
;                       (expand=>-clauses rest)))))))

; ;; this should replace the original cond in base-eval
; (define (install-cond=>-syntax)
;   (put-syntax! 'cond eval-cond=>)
;   (void))

; (install-cond=>-syntax)




(define (eval-cond=> exp env)
  (eval (cond=>->if exp) env))

(define (cond=>? exp)                     (tagged-list? exp 'cond))
(define (cond=>-clauses exp)              (mcdr exp))
(define (cond=>-else-clause? clause)      (eq? (cond=>-predicate clause) 'else))
(define (cond=>-predicate clause)         (mcar clause))
(define (cond=>-recipient clause)         (mcaddr clause))
(define (cond=>-recipient-clause? clause) (eq? (mcadr clause) '=>))


; this checks against the 2 forms for cond clauses
; 1) ((pred-clauses) (value-clauses)) -> result is (value-clauses)
; 2) ((pred-values) => proc)         -> result is (proc v)

(define (make-cond=>-recipient clause predicate)
  (mlist (cond=>-recipient clause) predicate))

(define (cond-consequent clause predicate)
  (if (cond=>-recipient-clause? clause)
      (make-cond=>-recipient clause predicate)
      (sequence->exp (cond=>-actions clause))))

(define (cond=>-actions clause)
  (mcdr clause))

(define (cond=>->if exp)
  (expand=>-clauses (cond=>-clauses exp)))

(define (expand=>-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (mcar clauses))
            (rest (mcdr clauses)))
        (if (cond=>-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond=>-actions first))
                (error "ELSE clause isn't last -- cond=>->if"
                       clauses))
            (let ((predicate (cond=>-predicate first)))
              (make-if predicate
                       (cond-consequent first predicate)
                       (expand=>-clauses rest)))))))

;; this should replace the original cond in base-eval
(define (install-cond=>-syntax)
  (put-syntax! 'cond eval-cond=>) 
  (void))
(install-cond=>-syntax)
