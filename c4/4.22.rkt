#lang racket

(require (rename-in racket/base 
                    [apply apply-in-underlying-scheme]
                    [eval eval-in-underlying-scheme]))

(require "lib/mutable.rkt")

;; Test procedures are run from a different file using rackunit 
(provide interpret)


;; Exercise 4.22
;;
; this version of the evaluator includes these expression types:
;  self-evaluating 
;  variable
;  quote 
;  define 
;  set! 
;  if
;  lambda 
;  begin 
;  cond + <test> => <recipient>
;  application? 
;  let
;  :TODO extend with let* and letrec?

(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond [(self-evaluating? exp) (analyze-self-evaluating exp)]
        [(quoted? exp) (analyze-quoted exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assignment? exp) (analyze-assignment exp)]
        [(definition? exp) (analyze-definition exp)]
        [(if? exp) (analyze-if exp)]
        [(lambda? exp) (analyze-lambda exp)]
        [(begin? exp) (analyze-sequence (begin-actions exp))]
        [(cond? exp) (analyze (cond->if exp))]
        [(let? exp) (analyze (let->combination exp))]
        [(application? exp) (analyze-application exp)]
        [else 
          (error "Unknown expression type -- ANALYZE" exp)]))

;; analyze functions

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
      (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
       (lambda (env)
               (set-variable-value! var (vproc env) env)
               'ok)))

(define (analyze-definition exp)
  (let ((var (definition-value exp))
        (vproc (analyze (definition-value exp))))
       (lambda (env) 
          (define-variable! var (vproc env) env)
          'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
      (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (mcar rest-procs))
              (mcdr rest-procs))))
  (let ((procs (mmap analyze exps)))
    (when (null? procs)
          (error "Empty sequence -- ANALYZE"))
    (loop (mcar procs) (mcdr procs))))


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (mmap analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (mmap (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond [(primitive-procedure? proc) (apply-primitive-procedure proc args)]
        [(compound-procedure? proc) 
         ((procedure-body proc) 
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc)))]
        [else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc)]))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (mcadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) (mlist->exp args)))

(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
          (apply-in-underlying-scheme procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment 
                          (procedure-parameters procedure)
                          arguments
                          (procedure-environment procedure)))]
        [else (error "Unknown procedure type -- APPLY" procedure)]))

;; helper functions 

(define (tagged-list? exp tag)
  (and (mpair? exp) (eq? (mcar exp) tag)))
(define (mlist-of-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (eval (first-operand exps) env)
             (mlist-of-values (rest-operands) env))))

(define (operator exp) (mcar exp))
(define (operands exp) (mcdr exp))
(define (no-operands? exp) (null? exp))
(define (first-operand exp) (mcar exp))
(define (rest-operands exp) (mcdr exp))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- LOOKUP-VARIABLE-VALUE" var)
        (let ((frame (first-frame env)))
            (cond [(bound? frame var) (frame-binding frame var)]
                  [(unassigned? frame var) (error "Unassigned variable -- LOOKUP-VARIABLE-VALUE" var)]
                  [else  (env-loop (enclosing-environment env))]
            ))))
  (env-loop env))

(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))
(define set-binding-in-frame! add-binding-to-frame!)
(define unbound  '*unbound*)
(define unassigned '*unassigned*)
(define (frame-binding frame var)
  (define (scan vars vals)
    (cond [(null? vars) unbound]
          [(eq? var (mcar vars)) (mcar vals)]
          [else (scan (mcdr vars) (mcdr vals))]))
  (scan (frame-variables frame) (frame-values frame)))

(define (bound? frame var)
  (not (eq? unbound (frame-binding frame var))))

(define (unassigned? frame var)
  (eq? unassigned (frame-binding frame var)))

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))
      ))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define boolean-expression-list mcar)

(define (eval-and exp env)
  (define (eval-expression-list exp)
    (cond [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) (eval-expression-list (rest-exps exp))]
          [else false]))
  (eval-expression-list (boolean-expression-list exp)))

(define (eval-or exp env)
  (eval (or->if (boolean-expression-list exp) env) env))

(define (or->if exp env)
  (cond [(last-exp? exp) (eval (first-exp exp) env)]
        [else (make-if (first-exp exp)
                       true
                       (or->if (rest-exps exp) env))]))

;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-initials exp) (mmap mcadr (mcadr exp)))
(define (let-parameters exp) (mmap mcar (mcadr exp)))
(define (let-body exp) (mcddr exp))
(define named-let-identifier mcar)

(define (named-let? exp) (symbol? (mcadr exp)))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (mcons (make-lambda (let-parameters exp)
                          (let-body exp))
             (let-initials exp))))

(define (named-let->combination exp)
  (let ((procedure-name (named-let-identifier exp)))
       (make-begin 
       (mlist 'define procedure-name
              (make-lambda (let-parameters exp)
                           (let-body exp)))
       (mcons procedure-name (let-initials exp)))))

;; quote
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (mcadr exp))
(define (eval-quote exp env) (text-of-quotation exp))

;; define
(define (definition? exp) (tagged-list? exp 'define))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
      (if (bound? frame var)
          (set-binding-in-frame! var val frame)
          (add-binding-to-frame! var val frame))))

(define (definition-variable exp)
  (if (symbol? (mcadr exp))
      (mcadr exp)
      (mcaadr exp)))

(define (definition-value exp)
  (if (symbol? (mcadr exp))
      (mcaddr exp)
      (make-lambda (mcdadr exp) (mcddr exp))))

;; set!
 
(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (mcadr exp))
(define (assignment-value exp) (mcaddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (if (bound? frame var)
                 (set-binding-in-frame! var val frame)
                 (env-loop (enclosing-environment env))))))
  (env-loop env))


;; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))
(define (if-predicate exp) (mcadr exp))
(define (if-consequent exp) (mcaddr exp))
(define (if-alternative exp) (if (not (null? (mcdddr exp)))
                                 (mcdddr exp)
                                 'false))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; anonymous procedures
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))
(define (lambda-parameters exp) (mcadr exp))
(define (lambda-body exp) (mcddr exp))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

;; sequences
(define (begin? exp) (tagged-list? exp 'begin))
(define (make-begin seq)(mcons 'begin seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
(define (begin-actions exp)(mcdr exp))
(define (last-exp? exp) (null? (mcdr exp)))
(define (first-exp seq)     (mcar seq))
(define (rest-exps seq)     (mcdr seq))

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps) env)]))


;; cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (make-cond-recipient clause predicate)
  (mlist (cond-recipient clause) predicate))

(define (cond-clauses exp)              (mcdr exp))
(define (cond-else-clause? clause)      (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)         (mcar clause))
(define (cond-actions clause)           (mcdr clause))
(define (cond-recipient clause)         (mcaddr clause))
(define (cond-recipient-clause? clause) (eq? (mcadr clause) '=>))
(define (cond-consequent clause predicate)
  (if (cond-recipient-clause? clause)
      (make-cond-recipient clause predicate)
      (sequence->exp (cond-actions clause))))

; this checks against the 2 forms for cond clauses
; 1) ((pred-clauses) (value-clauses)) -> result is (value-clauses)
; 2) ((pred-clauses) => proc)         -> result is (proc v)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (mcar clauses))
            (rest (mcdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((predicate (cond-predicate first)))
              (make-if predicate
                       (cond-consequent first predicate)
                       (expand-clauses rest)))))))

;; Loops
(define (while? exp) (tagged-list? exp 'while))

(define (eval-while exp env)
  (eval (while->combination exp) env))

(define while-predicate mcadr)
(define while-body      mcddr)

(define (while->combination exp)
  (sequence->exp
   (append (make-while-definition exp)
           (while-invocation))))

(define (make-while-definition exp)
  (mlist (append '(define (while-loop))
                 (make-while-body exp))))

(define (make-while-body exp)
  (mlist
   (make-if (while-predicate exp)
            (while->if-body exp)
            #f)))

(define (while->if-body exp)
  (sequence->exp (append (while-body exp)
                         (while-invocation))))

(define (while-invocation)
  (mlist '(while-loop)))

;; application

(define (application? exp) (mpair? exp))
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (mcadr p))
(define (procedure-body p)        (mcaddr p))
(define (procedure-environment p) (mcadddr p))

(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))
(define (extract-declaration exp)
  (define (scan-iter body scan-complete)
    (cond [(null? body) null]
          [(definition? (mcar body))
           (if scan-complete
               (error "define not allowed in an expression context")
               (mcons (mcar body)
                      (scan-iter (mcdr body) #f)))]
          [else (scan-iter (mcdr body) #t)]))
  (scan-iter exp #f))

(define (make-let-seq var-list body)
  (append (mlist 'let var-list) 
          body))

(define (make-let-unassigned define-list)
  (mmap (lambda (def)  
          (mlist (definition-variable def) 
                 (mlist 'quote '*unassigned*)))
        define-list))

(define (make-let-assigned variables values)
  (mmap (lambda (var val)  
          (mlist var val))
        variables
        values))

(define (make-set!-list variables values)
  (mmap (lambda (var val)  
          (mlist 'set! var val))
        variables
        values))

;; initialization
(define primitive-procedures
  (mlist (mlist '* *)
         (mlist '+ +)
         (mlist '- -)
         (mlist '/ /)
         (mlist '< <)
         (mlist '<= <=)
         (mlist '= =)
         (mlist '> >)
         (mlist '>= >=)
         (mlist 'abs abs)
         (mlist 'acos acos)
         (mlist 'append append)
         (mlist 'asin asin)
         (mlist 'assoc assoc)
         (mlist 'assq assq)
         (mlist 'assv assv)
         (mlist 'atan atan)
         (mlist 'boolean? boolean?)
         (mlist 'caaaar caaaar)
         (mlist 'caaadr caaadr)
         (mlist 'caaar caaar)
         (mlist 'caadar caadar)
         (mlist 'caaddr caaddr)
         (mlist 'caadr caadr)
         (mlist 'caar caar)
         (mlist 'cadaar cadaar)
         (mlist 'cadadr cadadr)
         (mlist 'cadar cadar)
         (mlist 'caddar caddar)
         (mlist 'cadddr cadddr)
         (mlist 'caddr caddr)
         (mlist 'cadr cadr)
         (mlist 'car car)
         (mlist 'cdaaar cdaaar)
         (mlist 'cdaadr cdaadr)
         (mlist 'cdaar cdaar)
         (mlist 'cdadar cdadar)
         (mlist 'cdaddr cdaddr)
         (mlist 'cdadr cdadr)
         (mlist 'cdar cdar)
         (mlist 'cddaar cddaar)
         (mlist 'cddadr cddadr)
         (mlist 'cddar cddar)
         (mlist 'cdddar cdddar)
         (mlist 'cddddr cddddr)
         (mlist 'cdddr cdddr)
         (mlist 'cddr cddr)
         (mlist 'cdr cdr)
         (mlist 'ceiling ceiling)
         (mlist 'char->integer char->integer)
         (mlist 'char-alphabetic? char-alphabetic?)
         (mlist 'char-ci<=? char-ci<=?)
         (mlist 'char-ci=? char-ci=?)
         (mlist 'char-ci>=? char-ci>=?)
         (mlist 'char-ci>? char-ci>?)
         (mlist 'char-downcase char-downcase)
         (mlist 'char-lower-case? char-lower-case?)
         (mlist 'char-numeric? char-numeric?)
         (mlist 'char-upcase char-upcase)
         (mlist 'char-upper-case? char-upper-case?)
         (mlist 'char-whitespace? char-whitespace?)
         (mlist 'char<=? char<=?)
         (mlist 'char=? char=?)
         (mlist 'char>=? char>=?)
         (mlist 'char>? char>?)
         (mlist 'char? char?)
         (mlist 'complex? complex?)
         (mlist 'cons cons)
         (mlist 'cos cos)
         (mlist 'display display)
         (mlist 'eq? eq?)
         (mlist 'equal? equal?)
         (mlist 'eqv? eqv?)
         (mlist 'eval eval)
         (mlist 'even? even?)
         (mlist 'exact? exact?)
         (mlist 'exp exp)
         (mlist 'expt expt)
         (mlist 'floor floor)
         (mlist 'for-each for-each)
         (mlist 'force force)
         (mlist 'gcd gcd)
         (mlist 'inexact? inexact?)
         (mlist 'integer->char integer->char)
         (mlist 'integer? integer?)
         (mlist 'lcm lcm)
         (mlist 'length length)
         (mlist 'list list)
         (mlist 'list->string list->string)
         (mlist 'list->vector list->vector)
         (mlist 'list-ref list-ref)
         (mlist 'list-tail list-tail)
         (mlist 'list? list?)
         (mlist 'log log)
         (mlist 'make-string make-string)
         (mlist 'make-vector make-vector)
         (mlist 'map map)
         (mlist 'max max)
         (mlist 'member member)
         (mlist 'memq memq)
         (mlist 'memv memv)
         (mlist 'min min)
         (mlist 'modulo modulo)
         (mlist 'negative? negative?)
         (mlist 'newline newline)
         (mlist 'not not)
         (mlist 'null? null?)
         (mlist 'number->string number->string)
         (mlist 'number? number?)
         (mlist 'odd? odd?)
         (mlist 'pair? pair?)
         (mlist 'positive? positive?)
         (mlist 'quotient quotient)
         (mlist 'rational? rational?)
         (mlist 'real? real?)
         (mlist 'remainder remainder)
         (mlist 'reverse reverse)
         (mlist 'round round)
         (mlist 'sin sin)
         (mlist 'sqrt sqrt)
         (mlist 'string string)
         (mlist 'string->list string->list)
         (mlist 'string->number string->number)
         (mlist 'string->symbol string->symbol)
         (mlist 'string-append string-append)
         (mlist 'string-ci<=? string-ci<=?)
         (mlist 'string-ci=? string-ci=?)
         (mlist 'string-ci>=? string-ci>=?)
         (mlist 'string-ci>? string-ci>?)
         (mlist 'string-copy string-copy)
         (mlist 'string-fill! string-fill!)
         (mlist 'string-length string-length)
         (mlist 'string-ref string-ref)
         (mlist 'string-set! string-set!)
         (mlist 'string<=? string<=?)
         (mlist 'string string)
         (mlist 'string=? string=?)
         (mlist 'string>=? string>=?)
         (mlist 'string>? string>?)
         (mlist 'string? string?)
         (mlist 'substring substring)
         (mlist 'symbol->string symbol->string)
         (mlist 'tan tan)
         (mlist 'truncate truncate)
         (mlist 'vector vector)
         (mlist 'vector->list vector->list)
         (mlist 'vector-fill! vector-fill!)
         (mlist 'vector-length vector-length)
         (mlist 'vector-ref vector-ref)
         (mlist 'vector-set! vector-set!)
         (mlist 'vector? vector?)
         (mlist 'write write)
         (mlist 'write-char write-char)
         (mlist 'zero? zero?) 
         ))
(define (primitive-procedure-names)
  (mmap mcar
        primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc)))
        primitive-procedures))


(define (setup-environment)
  (let ((initial-env 
          (extend-environment (primitive-procedure-names) 
                              (primitive-procedure-objects)
                              the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define the-global-environment (setup-environment))

(define (interpret exp)
  (mlist->exp (eval (exp->mlist exp) the-global-environment)))