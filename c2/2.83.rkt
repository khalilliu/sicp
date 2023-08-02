#lang sicp

;; Ex 2.83
;;
;; NB These procedures won't work because apply-generic is still the
;; version that uses generalised coercion which hasn't been implemented.
;; This version is using a raise strategy.
(define (apply-generic op . args)

  (define (all-coercable? coerce-procs)
    (not (member #f coerce-procs)))

  (define (coerce-args coercion-procs args)
    (map (lambda (coerce-proc arg)
           (coerce-proc arg))
         coercion-procs
         args))

  ; attempt to coerce all args into a common type among the args
  (define (apply-with-coercion arg-types)

    ; attempt to coerce all args using each tag-type in turn
    ; it's a scoped procedure to keep the original arguments (arg-types) for error reporting
    (define (coerce-types tags)
      (if (null? tags)   ; all targets exhausted
          (error "No method for these types - APPLY-GENERIC"
                 (list op arg-types))
          (let* ((target-type (car tags))
                 (arg-coercions (map        ; get all the coercion procedures from the target
                                 (lambda (coerce-from)
                                   (if (eq? coerce-from target-type)
                                       identity
                                       (get-coercion coerce-from target-type)))
                                 arg-types)))
            (if (all-coercable? arg-coercions)
                ; the target type is valid if all the args can be coerced
                (apply apply-generic
                       op
                       (coerce-args arg-coercions args))
                ; target-type is not valid, so try the next one in the list
                (coerce-types (cdr tags))))))        ; try the next target type

    (coerce-types arg-types))

  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (apply-with-coercion type-tags))))



;;
;; ======================================================================
;;
;; To test the exercises I need an implementation of put and get.
;; These are taken directly from section 3.3.3 of the book
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.3
;;
;; ======================================================================
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) cdr datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


;; ======================================================================
;;
;; The integer number package
;;
;; ======================================================================
(define (install-integer-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'integer x))

  ;; interface to rest of the system
  (put 'add    '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub    '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul    '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div    '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'equ?   '(integer integer) =)
  (put '=zero? '(integer integer) zero?)
  (put 'raise  '(integer) (lambda (n) (make-rational n 1)))
  (put 'make   'integer (lambda (n) (tag n)))
  (put 'type-level '(rational) (lambda (x) 1))
  'done)


;; ======================================================================
;;
;; The rational number package
;;
;; ======================================================================
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (let ((g (gcd n d)))
                           (cons (/ n g) (/ d g))))
  (define (add-rat x y) (make-rat (+ (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
  (define (sub-rat x y) (make-rat (- (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (* (numer x) (numer y))
                                  (* (denom x) (denom y))))
  (define (div-rat x y) (make-rat (* (numer x) (denom y))
                                  (* (denom x) (numer y))))
  (define (equ-rat x y) (and (equ? (numer x) (numer y))
                             (equ? (denom x) (denom y))))
  (define (=zero-rat x) (zero? (numer x)))
  (define (rational->real r) (make-real (/ (numer r) (denom r))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add    '(rational rational)  (lambda (x y) (tag (add-rat x y))))
  (put 'sub    '(rational rational)  (lambda (x y) (tag (sub-rat x y))))
  (put 'mul    '(rational rational)  (lambda (x y) (tag (mul-rat x y))))
  (put 'div    '(rational rational)  (lambda (x y) (tag (div-rat x y))))
  (put 'equ?   '(rational rational) equ-rat)
  (put '=zero? '(rational) =zero-rat)
  (put 'raise  '(rational) rational->real)
  (put 'make   'rational (lambda (n d) (tag (make-rat n d))))
  (put 'type-level '(rational) (lambda (x) 1))
  'done)


;; ======================================================================
;;
;; The real number package
;;
;; ======================================================================
(define (install-real-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'real x))
  (define (real->complex r) (make-complex-from-real-imag r 0))

  ;; interface to rest of the system
  (put 'add    '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub    '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul    '(real real) (lambda (x y) (tag (* x y))))
  (put 'div    '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ?   '(real real) =)
  (put '=zero? '(real real) zero?)
  (put 'raise  '(real) real->complex)
  (put 'make   'real (lambda (n) (tag n)))
  (put 'type-level '(real) (lambda (x) 3))
  'done)


;; ======================================================================
;;
;; The rectangular number package
;;
;; ======================================================================
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; ======================================================================
;;
;; The polar number package
;;
;; ======================================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; ======================================================================
;;
;; The complex number package
;;
;; ======================================================================
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (tag z) (attach-tag 'complex z))
  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2))
                                                   (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2))
                                                   (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                                 (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                                 (- (angle z1) (angle z2))))
  (define (equ-complex z1 z2) (and (equ? (magnitude z1) (magnitude z2))
                                   (equ? (angle z1) (angle z2))))
  (define (=zero-complex z1) (zero? (magnitude z1)))
  ;; interface to rest of the system
  (put 'add    '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub    '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul    '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div    '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ?   '(complex complex) equ-complex)
  (put '=zero? '(complex) =zero-complex)
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle     '(complex) angle)
  (put 'type-level '(complex) (lambda (x) 4))
  'done)


;; ======================================================================
;;
;; Generic procedures
;;
;; ======================================================================

; Constructors
(define (make-integer n)                  ((get 'make 'integer) n))
(define (make-real n)                     ((get 'make 'real) n))
(define (make-rational n d)               ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)   ((get 'make-from-mag-ang 'complex) r a))

; Selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))

; Operators
(define (add x y)     (apply-generic 'add x y))
(define (sub x y)     (apply-generic 'sub x y))
(define (mul x y)     (apply-generic 'mul x y))
(define (div x y)     (apply-generic 'div x y))
(define (equ? x y)    (apply-generic 'equ? x y))
(define (=zero? x)    (apply-generic '=zero? x))
(define (raise x)     (apply-generic 'raise x))
(define (type-level z) (apply-generic 'type-level z))

;; ======================================================================
;;
;; Package installation
;;
;; ======================================================================
(define (install-number-packages)
  (install-integer-package)
  (install-polar-package)
  (install-rectangular-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package))

(install-number-packages)
