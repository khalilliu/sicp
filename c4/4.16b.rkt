#lang racket

(require (except-in "lib/base-eval.rkt"
                    lookup-variable-value)
         "4.04.rkt"  ;; boolean expression
         "4.05.rkt"  ;; cond with =>
         "4.06.rkt"  ;; let
         "4.07.rkt"  ;; let*
         "4.08.rkt"  ;; named-let
         "4.09.rkt"  ;; while
         "4.13.rkt"  ;; unbinding)
)


(provide interpret)

;; Exercise 4.16a
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
;;  and =>> &&
;;  or =>> ||
;;  cond 
;;  special cond <test> => <recipient>
;;  let
;;  let*
;;  named let
;;  while
;;  internal bindings

;; NB using the pair of lists ((vars) (values)) rather than the
;; list of pairs ((var val) (var val) ...)

;; a
(define (frame-binding var frame)
  (let scan ((variables (mcar frame))
               (values (mcdr frame)))
      (cond [(or (null? variables) (null? values))
              'frame-binding-missing]
            [(eq? var (mcar variables)) (mcar values)]
            [else (scan (mcdr variables) (mcdr values))])))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (cond [(bound? var frame) (frame-binding var frame)]
                   [(unassigned? var frame) (error "Unassigned variable" var)]
                   [else (env-loop (enclosing-environment env))]))))
  (env-loop env))

(define (bound? var frame) 
  (mmemq var (mcar frame)))

(define (unassigned? var frame)
  (eq? '*unsassigned* (frame-binding var frame)))


;; b 
(define (scan-out-defines body)
  (let ((defined-vars (definitions body)))
       (if (null? defined-vars)
            body
            (mlist  
              (make-let-seq 
                (unassigned-definitions defined-vars)
                (unassigned-initialisations defined-vars)
                (scanned-body body))))))

(define (definitions exp)
  (define (scan-iter body in-sub-definition)
    (cond ((null? body) null)
          ((definition? (mcar body))
           (if in-sub-definition
               (error "define cannot appear in an expression context - DEFINITIONS" exp)
               (mcons (mcar body)
                     (scan-iter (mcdr body) #f))))
          (else (scan-iter (mcdr body) #t))))
  (scan-iter exp #f))

(define (make-let-seq unassigned-vars initial-values body)
  (mappend (mlist 'let unassigned-vars)
          initial-values
          body))

(define (unassigned-definitions define-list) 
  (mmap (lambda (def)
          (mlist (definition-variable def) '(quote *unassigned*)))
       define-list))

(define (unassigned-initialisations define-list)
  (mmap (lambda (def)
          (mlist 'set! (definition-variable def) 
                  (definition-value def)))
       define-list))

(define (scanned-body body)
  (cond [(null? body) body]
        [(definition? (mcar body)) (scanned-body (mcdr body))]
        [else (mcons (mcar body) (scanned-body (mcdr body)))]))


;; test

; (define exp (exp->mlist '(lambda (x)
;   (define u 1)
;   (define v 2)
;   ('body)))
; )

; (scan-out-defines (mcddr exp))
