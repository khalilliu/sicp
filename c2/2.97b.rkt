#lang sicp


;; Ex 2.97b
;;

;; ======================================================================
;;
;; The dense terms package 
;; (1 0 1 0 1) x^4 + x^2 + 1
;;
;; ======================================================================
(define (install-dense-terms-package)
  (define (tag terms) (attach-tag 'dense terms))
  (define (sparse-first-term terms)       ((get 'first-term 'sparse) terms))
  (define (sparse-rest-terms terms)       ((get 'rest-terms 'sparse) terms))
  (define (sparse-empty-termlist? terms)  ((get 'empty-termlist? 'sparse) terms))
  (define (sparse-the-empty-termlist)     ((get 'the-empty-termlist 'sparse)))

  (define (sparse->dense terms)
    (define (sparse-iter L)
      (cond ((sparse-empty-termlist? L) (the-empty-termlist))
            ((=zero? (coeff (sparse-first-term L))) (sparse-iter (sparse-rest-terms L)))
            (else (adjoin-term (sparse-first-term L)
                               (sparse-iter (sparse-rest-terms L))))))
    (sparse-iter terms))


  (define (make-poly variable term-list)
    ; leading zeros are not desirable
    (define (shrink-termlist terms)
      (cond ((empty-termlist? terms) (the-empty-termlist))
            ((=zero? (coeff (first-term terms)))
             (shrink-termlist (rest-terms terms)))
            (else terms)))
    (cons variable (tag (shrink-termlist term-list))))

  ; these procedures work with both term list representations and so they
  ; should be defined in the polynomial package
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  (define (the-empty-termlist) nil)
  (define (first-term term-list) (make-term (- (length term-list) 1)
                                            (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (equal? term-list (the-empty-termlist)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (coeff term) 
              (zero-pad-terms (order term) term-list))))

  (define (make-zero-terms n)
    (if (= n 0) 
        nil
        (cons 0 (make-zero-terms (- n 1)))))

  (define (zero-pad-terms max-order terms)
    (append (make-zero-terms (- max-order (length terms)))
            terms))
  
  (define (terms-zero? terms)
    (if (empty-termlist? terms)
        #t
        (and (=zero? (coeff (first-term terms)))
             (terms-zero? (rest-terms terms)))))
    
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))


   (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (term-map f terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (adjoin-term (f (first-term terms))
                     (term-map f (rest-terms terms)))))


   (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))

  (define (negate-terms p)
    (term-map negate-term p))

  (define (add-integer-terms i L)
    (add-terms (number->termlist i) L))


  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (sub-integer-terms i L)
    (sub-terms L (number->termlist i)))

  (define (sub-terms-integer i L)
    (sub-terms (number->termlist i) L))

  (define (mul-integer-terms i L)
    (mul-terms (number->termlist i) L))

  (define (number->termlist i)
    (adjoin-term (make-term 0 i) (the-empty-termlist)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1) 
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1)) (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (- (order t1) (order t2)))
                     (new-first-term (make-term new-o new-c)))
                (let ((rest-of-result (div-terms 
                                        (sub-terms 
                                          L1
                                          (mul-term-by-all-terms new-first-term L2))
                                        L2)))
                  (list (adjoin-term new-first-term (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (div-integer-terms i L)
    (div-terms (number->termlist i) L))

  (define (div-terms-integer L i)
    (div-terms L (number->termlist i)))

  (define (integerising-factor a b)
    (let ((tb (first-term b)))
      (expt (coeff tb)
            (-  (+ 1 
                  (order (first-term a)))
                (order (first-term b))))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms a b)
    (cadr (div-terms (mul-integer-terms 
                        (integerising-factor a b) a)
                      b)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (reduce-list L factor)
    (term-map (lambda (t) 
                (make-term (order t)
                            (/ (coeff t) factor))) 
      L))

  (define (reduce L)
    (reduce-list L (apply gcd L)))

  (define (reduce-terms n d)
    (let* ((gcd (gcd-terms n d))
           (nd-factor (integerising-factor n d))
           (nn (div-terms (mul-integer-terms nd-factor n) gcd))
           (dd (div-terms (mul-integer-terms nd-factor d) gcd))
           (result-factor (apply gcd (append (map coeff (car nn)) (map coeff (car dd))))))
      (list (reduce-list nn result-factor)
            (reduce-list dd result-factor))))

  ;; interface to rest of the system
  (put 'make-poly          'dense           (lambda (var terms)   (make-poly var terms)))
  (put 'add                '(dense dense)   (lambda (t1 t2)       (tag (add-terms t1 t2))))
  (put 'add                '(dense sparse)  (lambda (t1 t2)       (tag (add-terms t1 (sparse->dense t2)))))
  (put 'add                '(integer dense) (lambda (t1 t2)       (tag (add-integer-terms t1 t2))))
  (put 'add                '(dense integer) (lambda (t1 t2)       (tag (add-integer-terms t2 t1))))
  (put 'mul                '(dense dense)   (lambda (t1 t2)       (tag (mul-terms t1 t2))))
  (put 'mul                '(dense sparse)  (lambda (t1 t2)       (tag (mul-terms t1 (sparse->dense t2)))))
  (put 'mul                '(integer dense) (lambda (i t)         (tag (mul-integer-terms i t))))
  (put 'mul                '(dense integer) (lambda (t i)         (tag (mul-integer-terms t i))))
  (put '=zero?             '(dense)         (lambda (t)           (terms-zero? t)))
  (put 'sub                '(dense dense)   (lambda (t1 t2)       (tag (sub-terms t1 t2))))
  (put 'sub                '(dense sparse)  (lambda (t1 t2)       (tag (sub-terms t1 (sparse->dense t2)))))
  (put 'sub                '(integer dense) (lambda (i t)         (tag (sub-integer-terms i t))))
  (put 'sub                '(dense integer) (lambda (t i)         (tag (sub-terms-integer t i))))
  (put 'div                '(dense dense)   (lambda (t1 t2)       (map tag (div-terms t1 t2))))
  (put 'div                '(dense sparse)  (lambda (t1 t2)       (map tag (div-terms t1 (sparse->dense t2)))))
  (put 'div                '(integer dense) (lambda (i t)         (map tag (div-integer-terms i t))))
  (put 'div                '(dense integer) (lambda (t i)         (map tag (div-terms-integer t i))))
  (put 'negate             '(dense)         (lambda (t)           (tag (negate-terms t))))
  (put 'first-term         'dense           (lambda (t)           (first-term t)))
  (put 'rest-terms         'dense           (lambda (t)           (rest-terms t)))
  (put 'the-empty-termlist 'dense           (lambda ()            the-empty-termlist))
  (put 'empty-termlist?    'dense           (lambda (t)           (empty-termlist? t)))
  (put 'gcd                '(dense dense)   (lambda (t1 t2)       (tag (reduce (gcd-terms t1 t2)))))
  (put 'gcd                '(dense sparse)  (lambda (t1 t2)       (tag (reduce (gcd-terms t1 (sparse->dense t2))))))
  (put 'reduce             '(dense dense)   (lambda (t1 t2)       (map tag (reduce-terms t1 t2))))
  (put 'reduce             '(dense sparse)  (lambda (t1 t2)       (map tag (reduce-terms t1 (sparse->dense t2)))))

  'done)


;; ======================================================================
;;
;; The sparse terms package
;; ((3 1) (2 1)) x^3 + x^2
;;
;; ======================================================================
(define (install-sparse-terms-package)
  ;; internal procedures
  ;; representation of poly
  (define (tag terms) (attach-tag 'sparse terms))

  (define (dense-first-term terms)      ((get 'first-term 'dense) terms))
  (define (dense-rest-terms terms)      ((get 'rest-terms 'dense) terms))
  (define (dense-empty-termlist? terms) ((get 'empty-termlist? 'dense) terms))
  (define (dense-the-empty-termlist)    ((get 'the-empty-termlist 'dense)))

  (define (dense->sparse terms)
    (define (dense-iter L)
      (cond [(dense-empty-termlist? L) (the-empty-termlist)]
            [(=zero? (coeff (dense-first-term L))) (dense-iter (dense-rest-terms L))]
            [else (adjoin-term (dense-first-term L)
                               (dense-iter (dense-rest-terms L)))]))
    (dense-iter terms))

  (define (make-poly variable term-list)
    (cons variable (tag term-list)))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  (define (the-empty-termlist) nil)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (equal? term-list (the-empty-termlist)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (terms-zero? terms)
    (if (empty-termlist? terms)
        #t
        (and (=zero? (coeff (first-term terms)))
             (terms-zero? (rest-terms terms)))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (term-map f terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (adjoin-term (f (first-term terms))
                     (term-map f (rest-terms terms)))))

  (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))

  (define (negate-terms p)
    (term-map negate-term p))

  (define (add-integer-terms i L)
    (add-terms (number->termlist i) L))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (sub-integer-terms i L)
    (sub-terms L (number->termlist i)))

  (define (sub-terms-integer i L)
    (sub-terms (number->termlist i) L))

  (define (mul-integer-terms i L)
    (mul-terms (number->termlist i) L))

  (define (number->termlist i)
    (adjoin-term (make-term 0 i) (the-empty-termlist)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (- (order t1) (order t2)))
                     (new-first-term (make-term new-o new-c)))
                (let ((rest-of-result (div-terms
                                       (sub-terms
                                        L1
                                        (mul-term-by-all-terms new-first-term L2))
                                       L2)))
                  (list (adjoin-term new-first-term
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  (define (div-integer-terms i L)
    (div-terms (number->termlist i) L))

  (define (div-terms-integer L i)
    (div-terms (number->termlist i) L))

  (define (integerising-factor a b)
    (let ((tb (first-term b)))
      (expt (coeff tb)
            (- (+ 1
                  (order (first-term a)))
               (order tb)))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms a b)
    (cadr (div-terms (mul-integer-terms
                      (integerising-factor a b)
                      a)
                     b)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (reduce-list L factor)
    (term-map (lambda (t)
                (make-term (order t)
                           (/ (coeff t) factor)))
              L))

  (define (reduce L)
    (reduce-list L (apply gcd (map coeff L))))

  (define (reduce-terms n d)
    (let* ((gcd-nd (reduce (gcd-terms n d)))
           (nd-factor (integerising-factor n d))
           (nn (car (div-terms (mul-integer-terms nd-factor n) gcd-nd)))
           (nd (car (div-terms (mul-integer-terms nd-factor d) gcd-nd)))
           (nndd-list (append (map coeff nn) (map coeff nd)))
           (result-factor (apply gcd nndd-list)))
      (list (reduce-list nn result-factor)
            (reduce-list nd result-factor))))

  ;; interface to rest of the system
  (put 'make-poly          'sparse           (lambda (var terms)   (make-poly var terms)))
  (put 'add                '(sparse sparse)  (lambda (t1 t2)       (tag (add-terms t1 t2))))
  (put 'add                '(sparse dense)   (lambda (t1 t2)       (tag (add-terms t1 (dense->sparse t2)))))
  (put 'add                '(integer sparse) (lambda (t1 t2)       (tag (add-integer-terms t1 t2))))
  (put 'add                '(sparse integer) (lambda (t1 t2)       (tag (add-integer-terms t2 t1))))
  (put 'mul                '(sparse sparse)  (lambda (t1 t2)       (tag (mul-terms t1 t2))))
  (put 'mul                '(sparse dense)   (lambda (t1 t2)       (tag (mul-terms t1 (dense->sparse t2)))))
  (put 'mul                '(integer sparse) (lambda (i t)         (tag (mul-integer-terms i t))))
  (put 'mul                '(sparse integer) (lambda (t i)         (tag (mul-integer-terms t i))))
  (put '=zero?             '(sparse)         (lambda (t)           (terms-zero? t)))
  (put 'sub                '(sparse sparse)  (lambda (t1 t2)       (tag (sub-terms t1 t2))))
  (put 'sub                '(sparse dense)   (lambda (t1 t2)       (tag (sub-terms t1 (dense->sparse t2)))))
  (put 'sub                '(integer sparse) (lambda (i t)         (tag (sub-integer-terms i t))))
  (put 'sub                '(sparse integer) (lambda (t i)         (tag (sub-terms-integer t i))))
  (put 'div                '(sparse sparse)  (lambda (t1 t2)       (map tag (div-terms t1 t2))))
  (put 'div                '(sparse dense)   (lambda (t1 t2)       (map tag (div-terms t1 (dense->sparse t2)))))
  (put 'div                '(integer sparse) (lambda (i t)         (map tag (div-integer-terms i t))))
  (put 'div                '(sparse integer) (lambda (t i)         (map tag (div-terms-integer t i))))
  (put 'negate             '(sparse)         (lambda (t)           (tag (negate-terms t))))
  (put 'first-term         'sparse           (lambda (t)           (first-term t)))
  (put 'rest-terms         'sparse           (lambda (t)           (rest-terms t)))
  (put 'the-empty-termlist 'sparse           (lambda ()            the-empty-termlist))
  (put 'empty-termlist?    'sparse           (lambda (t)           (empty-termlist? t)))
  (put 'gcd                '(sparse sparse)  (lambda (t1 t2)       (tag (reduce (gcd-terms t1 t2)))))
  (put 'gcd                '(sparse dense)   (lambda (t1 t2)       (tag (reduce (gcd-terms t1 (dense->sparse t2))))))
  (put 'reduce             '(sparse sparse)  (lambda (t1 t2)       (map tag (reduce-terms t1 t2))))
  (put 'reduce             '(sparse dense)   (lambda (t1 t2)       (map tag (reduce-terms t1 (dense->sparse t2)))))


'done)


;; ======================================================================
;;
;; The polynomial package
;;
;; ======================================================================
(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))

  (define (make-dense-poly var terms) ((get 'make-poly 'dense) var terms))
  (define (make-sparse-poly var terms) ((get 'make-poly 'sparse) var terms))

  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define variable? symbol?)

  ;; use alphabetic order of the variable to decide which representation to use
  (define (var-order>? v1 v2)
    (string>? (symbol->string v1) (symbol->string v2)))

  (define (coerce-poly psrc ptarget)
    (let ((coerce-var (variable ptarget))
          (poly-constructor (if (eq? (type-tag (contents psrc)) 'dense)
                                make-dense-polynomial
                                make-sparse-polynomial))
          (zeroth-term (make-term 0 (tag psrc))))
        (contents (poly-constructor coerce-var (list zeroth-term )))))
  
  (define (add-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2))))
        ;; not same variable
        ((var-order>? (variable p1) (variable p2))
          (add-poly p1 (coerce-poly p2 p1)))
        (else (add-poly p2 (coerce-poly p1 p2)))))

  (define (mul-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2))))
        ((var-order>? (variable p1) (variable p2))
          (mul-poly p1 (coerce-poly p2 p1)))
        (else (mul-poly p2 (coerce-poly p1 p2)))))

  (define (poly-zero? p)
    (=zero? (term-list p)))
    
  (define (negate-poly p)
    (make-poly (variable p)
               (negate (term-list p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (sub-integer-poly i p)
    (make-poly (variable p)
               (sub i (term-list p))))

  (define (sub-poly-integer p i)
    (make-poly (variable p)
               (sub (term-list p) i)))

  (define (add-integer-poly i p)
    (make-poly (variable p)
               (add i (term-list p))))

  (define (mul-integer-poly i p)
    (make-poly (variable p)
               (mul i (term-list p))))

  (define (div-result var terms)
    (list (tag (make-poly var (car terms)))
          (tag (make-poly var (cadr terms)))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (div-result (variable p1) (div (term-list p1) (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (div-integer-poly i p)
    (div-result (variable p) (div i (term-list p))))

  (define (div-poly-integer p i)
    (div-result (variable p) (div (term-list p) i)))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (greatest-common-divisor (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-POLY" 
                (list p1 p2))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (div-result (variable p1) (reduce (term-list p1) (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (put 'make   'term                    (lambda (order coeff) (list order coeff)))
  (put 'make-dense-poly  'polynomial    (lambda (var terms)   (tag (make-dense-poly var terms))))
  (put 'make-sparse-poly 'polynomial    (lambda (var terms)   (tag (make-sparse-poly var terms))))
  (put 'add    '(polynomial polynomial) (lambda (p1 p2)       (tag (add-poly p1 p2))))
  (put 'add    '(integer polynomial)    (lambda (i p)         (tag (add-integer-poly i p))))
  (put 'add    '(polynomial integer)    (lambda (p i)         (tag (add-integer-poly i p))))
  (put 'mul    '(polynomial polynomial) (lambda (p1 p2)       (tag (mul-poly p1 p2))))
  (put 'mul    '(integer polynomial)    (lambda (i p)         (tag (mul-integer-poly i p))))
  (put 'mul    '(polynomial integer)    (lambda (p i)         (tag (mul-integer-poly i p))))
  (put '=zero? '(polynomial)            (lambda (p1)          (poly-zero? p1)))
  (put 'sub    '(polynomial polynomial) (lambda (p1 p2)       (tag (sub-poly p1 p2))))
  (put 'sub    '(integer polynomial)    (lambda (i p)         (tag (sub-integer-poly i p))))
  (put 'sub    '(polynomial integer)    (lambda (p i)         (tag (sub-poly-integer p i))))
  ; div doesn't return a poly - it returns a list of 2 polys
  (put 'div    '(polynomial polynomial) (lambda (p1 p2)       (div-poly p1 p2)))
  (put 'div    '(integer polynomial)    (lambda (i p)         (div-integer-poly i p)))
  (put 'div    '(polynomial integer)    (lambda (p i)         (div-poly-integer p i)))
  (put 'negate '(polynomial)            (lambda (p)           (tag (negate-poly p))))
  (put 'gcd    '(polynomial polynomial) (lambda (p1 p2)       (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial) (lambda (p1 p2)       (reduce-poly p1 p2)))

  'done)


;; ======================================================================

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "procedure not installed -- APPLY-GENERIC" op args))))


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

  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

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
  (put 'negate     '(integer)         (lambda (x)   (sub (make-integer 0) x)))
  (put 'gcd        '(integer integer) (lambda (x y) (tag (gcd x y))))
  (put 'reduce     '(integer integer) (lambda (x y) (map tag (reduce-integers x y))))

  'done)


;; ======================================================================
;;
;; The rational number package
;;
;; ======================================================================
(define (install-rational-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'rational x))

  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))


  (define (ratio r) 
    (if (rational-function? r)
      (error "RATIO: scalar ratio of polynomials not supported" r)
      (/ (numer r) (denom r))))

  (define (add-rat x y) (make-rat (add (mul (numer x) (denom y))
                                     (mul (numer y) (denom x)))
                                  (mul (denom x) (denom y))))
  (define (sub-rat x y) (make-rat (sub (mul (numer x) (denom y))
                                     (mul (numer y) (denom x)))
                                  (mul (denom x) (denom y))))
  (define (mul-rat x y) (make-rat (mul (numer x) (numer y))
                                  (mul (denom x) (denom y))))
  (define (div-rat x y) (make-rat (mul (numer x) (denom y))
                                  (mul (denom x) (numer y))))
  (define (equ-rat x y) (and (equ? (numer x) (numer y))
                             (equ? (denom x) (denom y))))
  (define (=zero-rat x) (=zero? (numer x)))
  (define (rational-function? r)
    (or (eq? 'polynomial (type-tag (numer r)))
        (eq? 'polynomial (type-tag (denom r)))))
  (define (rational->real r) (make-real (exact->inexact (ratio r))))
  (define (project r)
    (make-integer (truncate (ratio r))))

  ;; interface to rest of the system
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
  (put 'negate     '(rational)           (lambda (x)   (sub (tag (make-rat 0 1)) x)))

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
  (put '=zero?     '(real)       (lambda (x)   (zero? x)))
  (put 'raise      '(real)       (lambda (x)   (real->complex x)))
  (put 'type-level '(real)       (lambda (x)   3))
  (put 'project    '(real)       (lambda (x)   (project x)))
  (put 'sq-root    '(real)       (lambda (x)   (tag (sqrt x))))
  (put 'square     '(real)       (lambda (x)   (tag (* x x))))
  (put 'sine       '(real)       (lambda (x)   (tag (sin x))))
  (put 'cosine     '(real)       (lambda (x)   (tag (cos x))))
  (put 'arctan     '(real real)  (lambda (x y) (tag (atan x y))))
  (put 'negate     '(real)       (lambda (x)   (sub (tag (make 0)) x)))
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
  (put 'negate              '(complex)         (lambda (x) (sub (tag (make-from-real-imag 0 0)) x)))

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
; (define (make-complex-from-mag-ang r a)   ((get 'make-from-mag-ang 'complex) r a))
(define (make-polynomial var terms)       ((get 'make 'polynomial) var terms))
(define (make-term order coeff)           ((get 'make 'term) order coeff))
(define (make-dense-polynomial var terms)  ((get 'make-dense-poly  'polynomial) var terms))
(define (make-sparse-polynomial var terms) ((get 'make-sparse-poly 'polynomial) var terms))


; Selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))

; Operators
(define (add x y)                     (apply-generic 'add x y))
(define (sub x y)                     (apply-generic 'sub x y))
(define (mul x y)                     (apply-generic 'mul x y))
(define (div x y)                     (apply-generic 'div x y))
(define (equ? x y)                    (apply-generic 'equ? x y))
(define (=zero? x)                    (apply-generic '=zero? x))
(define (square x)                    (apply-generic 'square x))
(define (sq-root x)                   (apply-generic 'sq-root x))
(define (sine x)                      (apply-generic 'sine x))
(define (cosine x)                    (apply-generic 'cosine x))
(define (arctan x y)                  (apply-generic 'arctan x y))
(define (negate x)                    (apply-generic 'negate x))
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))
(define (reduce x y)                  (apply-generic 'reduce x y))


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
  (install-complex-package)
  (install-polynomial-package)
  (install-dense-terms-package)
  (install-sparse-terms-package))

(install-number-packages)



;; ======================================================================
;;
;; Test case
;;
;; ======================================================================
(define (show x) (newline) (display x))

(define ps1 (make-sparse-polynomial 'x '((1 1)(0 1))))
(define ps2 (make-sparse-polynomial 'x '((3 1)(0 -1))))
(define ps3 (make-sparse-polynomial 'x '((1 1))))
(define ps4 (make-sparse-polynomial 'x '((2 1)(0 -1))))

(define rfs1 (make-rational ps1 ps2))
(define rfs2 (make-rational ps3 ps4))

(show (add rfs1 rfs2))
; => (rational (polynomial x sparse (3 -1) (2 -2) (1 -3) (0 -1)) (polynomial x sparse (4 -1) (3 -1) (1 1) (0 1)))

