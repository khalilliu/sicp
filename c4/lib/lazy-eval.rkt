#lang racket

(require (rename-in racket/base 
                    [apply apply-in-underlying-scheme]
                    [eval eval-in-underlying-scheme])
         "mutable.rkt"
         "table.rkt")

(provide (all-defined-out)  ;; :TODO define explicitly which procedures are provided
         (all-from-out "mutable.rkt")
         (all-from-out "table.rkt"))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-syntax (type-tag exp)) ((get-syntax (type-tag exp)) exp env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp) 
                env))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure 
                                   (list-of-arg-values arguments env))) ;; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ;; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (actual-value (first-operand exps) env)
             (list-of-arg-values (rest-operands exps)
                                 env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (mcons (delay-it (first-operand exps) env)
             (list-of-delayed-args (rest-operands exps)
                                   env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (eval (first-operand exps) env)
             (list-of-values (rest-operands exps) env))))


;; lazy values

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-mcar! obj 'evaluated-thunk)
           (set-mcar! (mcdr obj) result)  ; replace exp with its value
           (set-mcdr! (mcdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (mlist 'thunk exp env))

(define (thunk? obj) 
  (tagged-list? obj 'thunk))
(define (evaluated-thunk? obj) 
  (tagged-list? obj 'evaluated-thunk))
(define thunk-exp mcadr)
(define thunk-env mcaddr)
(define thunk-value mcadr)

(define (eval-quoted exp env)
  (text-of-quotation exp))

;; eval if
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


;; eval begin
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; eval assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  (void))



(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  (void))

;; eval lambda

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

;; self-evaluating items are numbers and strings:

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

;; exp: (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (mcadr exp))

(define (tagged-list? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

;; assignment: (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (mcadr exp))
(define (assignment-value exp) (mcaddr exp))

;; definitions 
;; 1. (define <var> <value>)
;; 2. (define (<var> <param_1> ... <param_n>) <body>)
;; syntactic sugar for
;;  (define <var>  
;;    (lambda (<param_1> ... <param_n>)
;;     <body>))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (mcadr exp))
      (mcadr exp)
      (mcaadr exp)))
(define (definition-value exp)
  (if (symbol? (mcadr exp))
      (mcaddr exp)
      (make-lambda (mcdadr exp)   ; formal parameters
                   (mcddr exp)))) ; body

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (mcadr exp))
(define (lambda-body exp) (mcddr exp))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

;; if 
;; ('if <predicate> <consequent> <alternative>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (mcadr exp))
(define (if-consequent exp) (mcaddr exp))
(define (if-alternative exp)
  (if (not (null? (mcdddr exp)))
      (mcadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

;; begin
;; ('begin <action> ... <last-action>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (mcdr exp))
(define (last-exp? seq) (null? (mcdr seq)))
(define (first-exp seq) (mcar seq))
(define (rest-exps seq) (mcdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (mcons 'begin seq))


;; application 
;; (<application-name> ...arguments)
(define (application? exp) (mpair? exp))
(define (operator exp) (mcar exp))
(define (operands exp) (mcdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (mcar ops))
(define (rest-operands ops) (mcdr ops))


;; cond
;; ('cond (<predicate1> <action1>) ... ('else <else-action>))
;; equal to :
;; ('if <predicate1> <action1> 
;;    ('if <predicate2> <action2> <else-action>))
(define (cond? exp) (tagged-list? exp 'cond))
(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (cond-clauses exp)              (mcdr exp))
(define (cond-else-clause? clause)      (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)         (mcar clause))
(define (cond-recipient clause)         (mcaddr clause))
(define (cond-recipient-clause? clause) (eq? (mcadr clause) '=>))

(define (make-cond-recipient clause predicate)
  (mlist (cond-recipient clause) predicate))

(define (cond-consequent clause predicate)
  (if (cond-recipient-clause? clause)
      (make-cond-recipient clause predicate)
      (sequence->exp (cond-actions clause))))

(define (cond-actions clause)
  (mcdr clause))

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
                (error "ELSE clause isn't last -- cond->if"
                       clauses))
            (let ((predicate (cond-predicate first)))
              (make-if predicate
                       (cond-consequent first predicate)
                       (expand-clauses rest)))))))

;; Boolean
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;; Procedures
(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (mcadr p))
(define (procedure-body p) (mcaddr p))
(define (procedure-environment p) (mcadddr p))


;; Environments 
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

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))



(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


; Boolean expressions
(define boolean-expression-list mcdr)


(define (eval-and exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) true]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) (eval-expression-list (rest-exps exp))]
          [else false]))
  (eval-expression-list (boolean-expression-list exp)))

(define (eval-or exp env)
  (define (eval-expression-list exp)
    (cond [(null? exp) false]
          [(last-exp? exp) (eval (first-exp exp) env)]
          [(eval (first-exp exp) env) true]
          [else (eval-expression-list (rest-exps exp))]))
  (eval-expression-list (boolean-expression-list exp)))

; Let statements
; may be named or not named

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (named-let? exp) 
  (and (tagged-list? exp 'let)
       (symbol? (mcadr exp))))

;; Let selectors
(define (let-initials exp) (mmap mcadr (mcadr exp)))
(define (let-parameters exp) (mmap mcar (mcadr exp)))
(define named-let-identifier mcar)
(define let-body mcddr)

;; Named Let statements

(define (named-let->combination exp)
  (let ((procedure-name (named-let-identifier exp)))
    ; 2 expressions are needed so wrap them in a begin form
    (make-begin 
     (mlist
      ; define the procedure with the name given in the let expression
      (mlist 'define procedure-name 
            (make-lambda 
             (let-parameters exp) 
             (let-body exp)))
      ; apply the procedure with the initial values given by the let expression
      (mcons procedure-name (let-initials exp))))))


(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination (mcdr exp))
      (mcons (make-lambda (let-parameters exp) 
                          (let-body exp))
             (let-initials exp))))

;;; Let* statements
;;; a let* is syntactic sugar for nested lets
(define (let*->nested-lets exp)
  (define (make-let params)
    (cond ((last-exp? params) 
           (mappend (mlist 'let 
                           (mlist (mcar params)))
                    (let-body exp)))
          (else (mlist 'let 
                       (mlist (mcar params))
                       (make-let (mcdr params))))))
  (make-let (mcadr exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
;;; Letrec statements
(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(define letrec-declarations mcadr)
(define letrec-variable     mcar)
(define letrec-value        mcadr)
(define letrec-body         mcddr)
(define (letrec-initials exp) (mmap mcadr exp))

(define (letrec->let exp)
  (let* ((declarations (letrec-declarations exp))
         (initials     (letrec-initials declarations))) 
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

(define (letrec-unassigned-initialisations declarations)
  (mmap (lambda (dec)  
          (mlist 'set! 
                 (letrec-variable dec) 
                 (letrec-value dec)))
        declarations))

(define (make-let-seq unassigned-vars initial-values body)
  (mappend (mlist 'let unassigned-vars)
           initial-values 
           body))



;; install all syntaxs to syntax-table

(define (install-syntax)
  (put-syntax! 'quote eval-quoted) 
  (put-syntax! 'define eval-definition) 
  (put-syntax! 'set! eval-assignment) 
  (put-syntax! 'if eval-if) 
  (put-syntax! 'lambda eval-lambda) 
  (put-syntax! 'begin eval-begin) 
  (put-syntax! 'cond eval-cond) 
  (put-syntax! 'or eval-or)
  (put-syntax! 'and eval-and)
  (put-syntax! 'let eval-let) 
  (put-syntax! 'let* eval-let*)
  (put-syntax! 'letrec eval-letrec)
  (void))
(install-syntax)


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
;         (mlist 'for-each for-each)
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
;         (mlist 'map map)
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcadr proc))


(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) (mlist->exp args)))

(define (interpret exp)
  (let ((result (eval (exp->mlist exp) the-global-environment)))
    (mlist->exp (if (thunk? result)
                    (force-it result)
                    result))))



;; ===============
;;     REPL
;; ===============
;
; For convenience in running the metacircular evaluator, we provide a driver loop that models 
; the read-eval-print loop of the underlying Lisp system. 
; It prints a prompt, reads an input expression, evaluates this expression in the global environment, and prints the result. 
; We precede each printed result by an output prompt so as to distinguish 
; the value of the expression from other output that may be printed
;

;;; (define input-prompt ";;; L-Eval input:")
;;; (define output-prompt ";;; L-Eval value:")

;;; (define (driver-loop)
;;;   (prompt-for-input input-prompt)
;;;   (let ((input (read)))
;;;     (let ((output
;;;            (actual-value input the-global-environment)))
;;;       (announce-output output-prompt)
;;;       (user-print output)))
;;;   (driver-loop))

;;; (define (prompt-for-input string)
;;;   (newline) (newline) (display string) (newline))

;;; (define (announce-output string)
;;;   (newline) (display string) (newline))

;;; ; We use a special printing procedure, user-print, to avoid printing 
;;; ; the environment part of a compound procedure, which may be a very long list
;;; ; (or may even contain cycles).

;;; (define (user-print object)
;;;   (if (compound-procedure? object)
;;;       (display (list 'compound-procedure
;;;                      (procedure-parameters object)
;;;                      (procedure-body object)
;;;                      '<procedure-env>))
;;;       (display object)))

