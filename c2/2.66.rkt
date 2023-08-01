#lang sicp

(define (look-up given-key set-of-records)
  (if (null? set-of-records) #f
      (let ((parent (entry set-of-records)))
        (cond [(eq? parent nil) #f]
              [(equal? parent given-key) #t]
              [else
               (look-up given-key
                        (if (< given-key parent)
                            (left-branch set-of-records)
                            (right-branch set-of-records)))]))))