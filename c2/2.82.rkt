#lang sicp

(define (apply-genric op . args)
  
   (define (coerce object target-type)
     ;; => (do-coercion object->target) or (object)
     (let ((coercion (get-coercion (type-tag object) target-type))) 
       (if coercion (coercion object) object))) 

  (define (iter before-reference reference after-reference)

    (define (coerce-map objects)
      (map (lambda (object)
             (coerce object (type-tag (car reference))))
           objects))
    
    (let ((args (append before-reference reference after-reference)))
      (let ((proc (get op (map type-tag args))))
        (cond
          [(not (null? proc))
           (apply proc (map contents args))]
          [(null? reference)
            (error "No method for these types" 
                   (list op (map type-tag args)))]
          [else
           (let ((before-coerced (coerce-map before-reference))
                 (after-coerced (coerce-map after-reference)))
             (cond
               [(null? after-reference)
                (iter (append before-coerced reference)) nil nil]
               [else
                (iter (append before-coerced reference)
                      (list (car after-coerced))
                      (cdr after-coerced))]))])))
    )
  (iter nil (list (car args)) (cdr args))
 )