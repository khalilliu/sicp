#lang racket

(require "lib/base-eval.rkt"
         "4.16c.rkt"  ;; internal bindings
         ) 

(provide interpret)


;; Exercise 4.20a
;;
;; this version of the evaluator includes these expression types:
;;  self-evaluating 
;;  variable
;;  quote 
;;  define =>> fun
;;  set! 
;;  if
;;  lambda 
;;  begin 
;;  application? 
;;  and 
;;  or 
;;  cond 
;;  special cond <test> => <recipient>
;;  let
;;  let*
;;  named let
;;  while
;;  letrec

; letrec - transform to let with an initial value of '*unassigned* and a set! 
; to allow recursive and simultaneous definitions

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(define letrec-declarations mcadr)
(define letrec-variable mcar)
(define letrec-value mcadr)
(define letrec-body mcddr)
(define (letrec-initials exp) (mmap mcadr exp))

(define (letrec->let exp)
  (let* ((declarations (letrec-declarations exp))
         (initials (letrec-initials declarations)))
    (if (null? declarations)
        exp
        (make-let-seq 
          (letrec-unassigned-definitions declarations)
          (letrec-unassigned-initialisations declarations)
          (letrec-body exp)))))

(define (letrec-unassigned-definitions define-list)
  (mmap (lambda (def) 
                (mlist (letrec-variable def)
                       (mlist 'quote '*unassigned*))) 
        define-list))

(define (letrec-unassigned-initialisations define-list)
  (mmap (lambda (def)
                (mlist 'set! 
                        (letrec-variable def)
                        (letrec-value def)))
                define-list))

(define (make-let-seq unassigned-vars initial-values body)
  (mappend (mlist 'let unassigned-vars)
            initial-values
            body))

(define (install-letrec-syntax)
  (put-syntax! 'letrec eval-letrec)
  (void))

(install-letrec-syntax)





(define exp '(define (f x)
  (letrec ((even? 
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
            (odd? 
              (lambda (n)
                (if (= n 0)
                    false
                    (even? (- n 1))))))
  (display (even? x))
)))

(interpret exp)
(interpret '(f 12))





   (define (f x)
     (let ((new-even?
            (lambda (n)
              (if (= n 0)
                  true
                  (new-odd? (- n 1)))))
           (new-odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (new-even? (- n 1))))))
       "rest of body of f"
       (display (new-even? x))))
   

 
  ;  (define (f x)
  ;    ((lambda (new-even? new-odd?)
  ;       "rest of body of f"
  ;       (display (new-even? x)))   
  ;     (lambda (n)
  ;       (if (= n 0)
  ;           true
  ;           (new-odd? (- n 1))))
  ;     (lambda (n)
  ;       (if (= n 0)
  ;           false
  ;           (new-even? (- n 1))))))


(f 12)