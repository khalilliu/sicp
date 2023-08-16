#lang racket

(require "lib/amb-eval.rkt")

;; Exercise 4.45 version a
(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define nouns '(noun student professor cat class)))
(interpret '(define verbs '(verb studies lectures eats sleeps)))
(interpret '(define articles '(article the a)))
(interpret '(define prepositions '(prep for to in by with)))

(interpret '(define (parse-sentence)
              (list 'sentence
                    (parse-noun-phrase)
                    (parse-verb-phrase))))

(interpret '(define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (amb noun-phrase
                     (maybe-extend (list 'noun-phrase
                                         noun-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-noun-phrase))))

(interpret '(define (parse-simple-noun-phrase)
              (list 'simple-noun-phrase
                    (parse-word articles)
                    (parse-word nouns))))

(interpret '(define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (amb verb-phrase
                     (maybe-extend (list 'verb-phrase
                                         verb-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-word verbs))))


(interpret '(define (parse-prepositional-phrase)
              (list 'prep-phrase
                    (parse-word prepositions)
                    (parse-noun-phrase))))

(interpret '(define (parse-word word-list)
              (require (not (null? *unparsed*)))
              (let ((found-word (random-word (cdr word-list))))
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word))))

(interpret '(define (random-word word-list)
              (list-ref word-list (random (length word-list)))))

(interpret '(define *unparsed* '()))
(interpret '(define (parse input)
              (set! *unparsed* input)
              (let ((sent (parse-sentence)))
                (require (null? *unparsed*))
                sent)))

(driver-loop)

;;; ;;; Amb-Eval input:
;;; (parse '(the professor lectures to the cat in the class))

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun student))
;;;  (verb-phrase
;;;   (verb-phrase
;;;    (verb eats)
;;;    (prep-phrase (prep to) (simple-noun-phrase (article the) (noun class))))
;;;   (prep-phrase (prep in) (simple-noun-phrase (article a) (noun professor)))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun student))
;;;  (verb-phrase
;;;   (verb eats)
;;;   (prep-phrase
;;;    (prep to)
;;;    (noun-phrase
;;;     (simple-noun-phrase (article the) (noun class))
;;;     (prep-phrase
;;;      (prep with)
;;;      (simple-noun-phrase (article the) (noun student)))))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (noun-phrase
;;;   (simple-noun-phrase (article the) (noun student))
;;;   (prep-phrase (prep in) (simple-noun-phrase (article a) (noun student))))
;;;  (verb-phrase
;;;   (verb lectures)
;;;   (prep-phrase (prep to) (simple-noun-phrase (article a) (noun professor)))))

;;; the student in a student lectures to a professor

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (noun-phrase
;;;   (noun-phrase
;;;    (simple-noun-phrase (article the) (noun student))
;;;    (prep-phrase (prep in) (simple-noun-phrase (article a) (noun student))))
;;;   (prep-phrase (prep by) (simple-noun-phrase (article a) (noun class))))
;;;  (verb eats))

;;; the student in a student by a class eats

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (noun-phrase
;;;   (simple-noun-phrase (article the) (noun student))
;;;   (prep-phrase
;;;    (prep in)
;;;    (noun-phrase
;;;     (simple-noun-phrase (article a) (noun student))
;;;     (prep-phrase
;;;      (prep with)
;;;      (simple-noun-phrase (article the) (noun professor))))))
;;;  (verb studies))

;;; the student in a student with the professor studies

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (parse '(the professor lectures to the cat in the class))


;;; ;;; Amb-Eval input:
;;; try-again