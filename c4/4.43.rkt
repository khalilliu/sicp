#lang racket

(require "lib/amb.rkt")

;; Exercise 4.43

(define (sailors)
  (define father first)
  (define daughter second)
  (define yacht third)
  (define (different-names father)
    (not (eq? (daughter father) (yacht father))))
  
  (let ((moore (list 'moore (amb 'mary-ann 'gabrielle 'lorna 'rosalind) 'lorna))
        (hood  (list 'hood 'melissa 'gabrielle))
        (hall (list 'hall (amb 'gabrielle 'mary-ann 'lorna) 'rosalind))
        (downing (list 'downing (amb 'gabrielle 'mary-ann 'lorna 'rosalind) 'melissa))
        (parker (list 'parker (amb 'gabrielle 'lorna 'rosalind) 'mary-ann)))
    (let ((gabrielle-father (amb hall downing parker))
          (lorna-father (amb hall downing parker)))
      (require (eq? (daughter gabrielle-father) 'gabrielle))
      (require (eq? (daughter lorna-father) 'lorna))
      (require (different-names lorna-father))
      (require (different-names gabrielle-father))
      (require (eq? (daughter parker) 
                    (yacht gabrielle-father)))
      lorna-father)))

;;; (display (sailors))
;;; (downing lorna melissa)



(define (sailors-2)
  
  (define father first)
  (define daughter second)
  (define yacht third)
  (define (different-names father)
    (not (eq? (daughter father) (yacht father))))
  
  (let ((moore   (list 'moore (amb 'mary-ann 'gabrielle 'lorna 'rosalind) 'lorna))
        (hood    (list 'hood  'melissa                 'gabrielle))
        (hall    (list 'hall  (amb 'gabrielle 'mary-ann 'lorna)  'rosalind))
        (downing (list 'downing (amb 'gabrielle 'mary-ann 'lorna 'rosalind) 'melissa))
        (parker  (list 'parker  (amb 'gabrielle 'lorna  'rosalind) 'mary-ann)))
    (require (different-names moore))
    (let* ((gabrielle-father (amb hall downing parker moore))
           (lorna-father (amb hall downing parker moore))
           (fathers (list moore hood hall downing parker))
           (daughters (map daughter fathers))
           (yachts    (map yacht    fathers)))
      (require (distinct? daughters))
      (require (distinct? yachts))
      (require (not (equal? daughters yachts)))
      (require (eq? (daughter gabrielle-father) 'gabrielle))
      (require (eq? (daughter lorna-father) 'lorna))
      (require (different-names lorna-father))
      (require (different-names gabrielle-father))
      (require (eq? (daughter parker)
                    (yacht gabrielle-father)))
      (newline)(display lorna-father))))


(sailors-2)
(try-again)
;;; (downing lorna melissa)
;;; (parker lorna mary-ann)