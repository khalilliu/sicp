#lang sicp


;; Ex 2.86
;;
(define (apply-generic op . args)

  ; only certain operations will result in an answer that can be
  ; projected e.g. boolean values - it makes no sense to project
  ; the result of calling =zero?
  (define (reduce-type x)
    (cond ((eq? op 'add)     (drop x))
          ((eq? op 'sub)     (drop x))
          ((eq? op 'mul)     (drop x))
          ((eq? op 'div)     (drop x))
          ((eq? op 'square)  (drop x))
          ((eq? op 'sq-root) (drop x))
          ((eq? op 'sine)    (drop x))
          ((eq? op 'cosine)  (drop x))
          ((eq? op 'arctan)  (drop x))
          (else x)))

  ; find the highest type level of a list of arguments
  (define (highest-type-level args)
    (if (null? args)
        0
        (let ((level (type-level (car args)))
              (highest (highest-type-level (cdr args))))
          (if (> level highest)
              level
              highest))))

  ; raise arg to the same level as target-type-level
  (define (raise-to arg target-type-level)
    (define (raise-iter current-arg)
      (let ((arg-level (type-level current-arg)))
        (cond ((= arg-level target-type-level) current-arg)
              ((< arg-level target-type-level) (raise-iter (apply-generic 'raise current-arg)))
              (else (error "Cannot raise argument to a lower type target" arg target-type-level)))))
    (raise-iter arg))

  ; raise all args to a common type (the highest in the tower of types)
  ; and apply the operator to them
  (define (apply-with-raised-types args)
    (let ((target-type-level (highest-type-level args)))
      (apply apply-generic
             op
             (map (lambda (arg)
                    (raise-to arg target-type-level))
                  args))))

  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (reduce-type (apply proc (map contents args)))
        (apply-with-raised-types args))))


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

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'integer)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'integer)
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

  ; complex numbers can now have parts which are
  ; made from sub-types within our system and so
  ; it's now possible to call raise with an argument
  ; that is a constant integer, rational or a real.
  (define (raise x)
    (cond ((integer? x)  (make-rational x 1))
          ((rational? x) (make-rational (numerator x) (denominator x)))
          ((real? x)     (raise (rationalize x 1/100000)))))

  ;; interface to rest of the system
  (put 'make       'integer           (lambda (x)   (tag x)))
  (put 'add        '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub        '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul        '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div        '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'equ?       '(integer integer) (lambda (x y) (= x y)))
  (put '=zero?     '(integer)         (lambda (x)   (zero? x)))
  (put 'raise      '(integer)         (lambda (x)   (make-rational x 1)))
  (put 'type-level '(integer)         (lambda (x)   1))
  ; does it make any sense to have sine, cosine, sq-root and arctan for integer?
  (put 'sq-root    '(integer)         (lambda (x)   (make-real (sqrt x))))
  (put 'square     '(integer)         (lambda (x)   (tag (* x x))))
  (put 'sine       '(integer)         (lambda (x)   (make-real(sin x))))
  (put 'cosine     '(integer)         (lambda (x)   (make-real(cos x))))
  (put 'arctan     '(integer integer) (lambda (x y) (make-real(atan x y))))

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
  (define (make-rat n d)
    (if (integer? n)
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (let ((rat (rationalize (/ n d) 1/100000)))
          (make-rat (numerator rat)
                    (denominator rat)))))

  (define (ratio x) (/ (numer x) (denom x)))

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
  (define (rational->real r) (make-real (exact->inexact (ratio r))))
  (define (project r)
    (make-integer (truncate (ratio r))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'make       'rational             (lambda (n d) (tag (make-rat n d))))
  (put 'add        '(rational rational)  (lambda (x y) (tag (add-rat x y))))
  (put 'sub        '(rational rational)  (lambda (x y) (tag (sub-rat x y))))
  (put 'mul        '(rational rational)  (lambda (x y) (tag (mul-rat x y))))
  (put 'div        '(rational rational)  (lambda (x y) (tag (div-rat x y))))
  (put 'equ?       '(rational rational)  (lambda (x y) (equ-rat x y)))
  (put '=zero?     '(rational)           (lambda (x)   (=zero-rat x)))
  (put 'raise      '(rational)           (lambda (x)   (rational->real x)))
  (put 'type-level '(rational)           (lambda (x)   2))
  (put 'project    '(rational)           (lambda (x)   (project x)))
  (put 'sq-root    '(rational)           (lambda (x)   (make-real (sqrt (ratio x)))))
  (put 'square     '(rational)           (lambda (x)   (tag (mul-rat x x))))
  (put 'sine       '(rational)           (lambda (x)   (make-real (sin (ratio x)))))
  (put 'cosine     '(rational)           (lambda (x)   (make-real (cos (ratio x)))))
  (put 'arctan     '(rational rational)  (lambda (x y) (make-real (atan (ratio x) (ratio y)))))

  'done)


;; ======================================================================
;;
;; The real number package
;;
;; ======================================================================
(define (install-real-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'real x))

  ; all complex types are created from lower types
  ; and so must be tagged using generic constructors
  (define (real->complex r)
    (make-complex-from-real-imag (make-real r)
                                 (make-real 0)))

  (define (project r)
    (cond ((integer? r) (make-rational r 1))
          (else (let ((rat (rationalize r 1/100000)))
                  (make-rational (numerator rat)
                                 (denominator rat))))))
  (define (make x)
    (let ((type (type-tag x))
          (val  (contents x)))
      (cond ((eq? type 'integer)  (tag x))
            ((eq? type 'rational) (tag (raise x)))
            ((eq? type 'real)     x)
            (else (error "MAKE-REAL : Bad type argument : " x)))))

  ;; interface to rest of the system
  (put 'make        'real        (lambda (x)   (make x)))
  (put 'add         '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub         '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul         '(real real) (lambda (x y) (tag (* x y))))
  (put 'div         '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ?        '(real real) (lambda (x y) (= x y)))
  (put '=zero?      '(real real) (lambda (x)   (zero? x)))
  (put 'raise      '(real)       (lambda (x)   (real->complex x)))
  (put 'type-level '(real)       (lambda (x)   3))
  (put 'project    '(real)       (lambda (x)   (project x)))
  (put 'sq-root    '(real)       (lambda (x)   (tag (sqrt x))))
  (put 'square     '(real)       (lambda (x)   (tag (* x x))))
  (put 'sine       '(real)       (lambda (x)   (tag (sin x))))
  (put 'cosine     '(real)       (lambda (x)   (tag (cos x))))
  (put 'arctan     '(real real)  (lambda (x y) (tag (atan x y))))
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
  ; internally rectangular numbers are (real . imag)
  ; and each part can be any of the lower types in the tower
  ; so all calcuations must use generic functions
  (define (magnitude z)
    (sq-root (add (square (real-part z))
                  (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'make-from-real-imag 'rectangular   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'rectangular   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part           '(rectangular) (lambda (x)   (real-part x)))
  (put 'imag-part           '(rectangular) (lambda (x)   (imag-part x)))
  (put 'magnitude           '(rectangular) (lambda (x)   (magnitude x)))
  (put 'angle               '(rectangular) (lambda (x)   (angle x)))

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

  ; internally polar numbers are (mag . ang)
  ; and each part can be any of the lower types in the tower
  ; so all calcuations must use generic functions
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (make-from-mag-ang (sq-root (add (square x) (square y)))
                       (arctan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'polar   (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part           '(polar) (lambda (x)   (real-part x)))
  (put 'imag-part           '(polar) (lambda (x)   (imag-part x)))
  (put 'magnitude           '(polar) (lambda (x)   (magnitude x)))
  (put 'angle               '(polar) (lambda (x)   (angle x)))

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
  (define (add-complex z1 z2) (make-from-real-imag (add (real-part z1) (real-part z2))
                                                   (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (sub (real-part z1) (real-part z2))
                                                   (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                                 (add (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                                                 (sub (angle z1) (angle z2))))
  (define (equ-complex z1 z2) (and (equ? (magnitude z1) (magnitude z2))
                                   (equ? (angle z1) (angle z2))))
  (define (=zero-complex z1) (zero? (magnitude z1)))
  (define (project z1)
    (make-real (real-part z1)))

  ;; interface to rest of the system
  (put 'make-from-real-imag 'complex           (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'complex           (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'add                 '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub                 '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul                 '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div                 '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'equ?                '(complex complex) (lambda (x y) (equ-complex x y)))
  (put '=zero?              '(complex)         (lambda (x)   (=zero-complex x)))
  (put 'real-part           '(complex)         (lambda (x)   (real-part x)))
  (put 'imag-part           '(complex)         (lambda (x)   (imag-part x)))
  (put 'magnitude           '(complex)         (lambda (x)   (magnitude x)))
  (put 'angle               '(complex)         (lambda (x)   (angle x)))
  (put 'type-level          '(complex)         (lambda (x)   4))
  (put 'project             '(complex)         (lambda (x)   (project x)))

  'done)

;; ======================================================================
;;
;; Type handling
;;
;; ======================================================================
(define (raise x)      (apply-generic 'raise x))
(define (type-level z) (apply-generic 'type-level z))
(define (project z)    (apply-generic 'project z))
(define (drop z)
  (if (= (type-level z) 1)
      z
      (let ((projected (project z)))
        (if (equ? z (raise projected))
            (drop projected)
            z))))

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
(define (square x)    (apply-generic 'square x))
(define (sq-root x)   (apply-generic 'sq-root x))
(define (sine x)      (apply-generic 'sine x))
(define (cosine x)    (apply-generic 'cosine x))
(define (arctan x y)  (apply-generic 'arctan x y))

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

(define (show x)
  (newline)
  (display x))

(define an-int (make-integer 4))
(define a-real (make-real 0.65))
(define a-rat  (make-rational 7 10))
(define a-complex-mag-ang   (make-complex-from-mag-ang   (make-integer 5) (make-real 0.75)))
(define a-complex-real-imag (make-complex-from-real-imag (make-rational 7 2) (make-integer 9)))

(show an-int)
(show a-rat)
(show a-real)
(show a-complex-mag-ang)
(show a-complex-real-imag)
(show (add an-int a-complex-real-imag))
(show (sub a-real a-complex-mag-ang))
(show (mul a-rat a-complex-mag-ang))
(show (div a-complex-real-imag a-complex-mag-ang))
