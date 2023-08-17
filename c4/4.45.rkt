#lang racket

(require "lib/amb-eval.rkt")

;; Exercise 4.45

;;; (define nouns '(nouns student professor cat class))
;;; (define verbs '(verbs studies lectures eats sleeps))
;;; (define articles '(articles the a))
;;; (define prepositions '(prep for to in by with))

;;; (define (parse-sentence) 
;;;             (list 'sentence  
;;;                   (parse-noun-phrase)
;;;                   (parse-verb-phrase)))

;;; (define (parse-noun-phrase)
;;;           (define (maybe-extend noun-phrase)
;;;             (amb noun-phrase
;;;                   (maybe-extend (list 'noun-phrase
;;;                                       noun-phrase
;;;                                       (parse-prepositional-phrase)))))
;;;           (maybe-extend (parse-simple-noun-phrase)))

;;; (define (parse-simple-noun-phrase)
;;;               (list 'simple-noun-phrase
;;;                     (parse-word articles)
;;;                     (parse-word nouns)))

;;; (define (parse-verb-phrase)
;;;               (define (maybe-extend verb-phrase)
;;;                 (amb verb-phrase
;;;                      (maybe-extend (list 'verb-phrase
;;;                                          verb-phrase
;;;                                          (parse-prepositional-phrase)))))
;;;               (maybe-extend (parse-word verbs)))


;;; (define (parse-prepositional-phrase)
;;;               (list 'prep-phrase
;;;                     (parse-word prepositions)
;;;                     (parse-noun-phrase)))

;;; (define (parse-word word-list)
;;;               (require (not (null? *unparsed*)))
;;;               (require (memq (car *unparsed*) (cdr word-list)))
;;;               (let ((found-word (car *unparsed*)))
;;;                 (set! *unparsed* (cdr *unparsed*))
;;;                 (list (car word-list) found-word)))


;;; (define *unparsed* '())
;;; (define (parse input)
;;;               (set! *unparsed* input)
;;;               (let ((sent (parse-sentence)))
;;;                 (require (null? *unparsed*))
;;;                 sent))



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
              (require (memq (car *unparsed*) (cdr word-list)))
              (let ((found-word (car *unparsed*)))
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word))))

(interpret '(define *unparsed* '()))
(interpret '(define (parse input)
              (set! *unparsed* input)
              (let ((sent (parse-sentence)))
                (require (null? *unparsed*))
                sent)))

(driver-loop)

;;; ;;; Amb-Eval input:
;;; (parse '(the professor lectures to the student in the class with the cat))

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb-phrase
;;;    (verb-phrase
;;;     (verb lectures)
;;;     (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;;    (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb-phrase
;;;    (verb lectures)
;;;    (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;;   (prep-phrase
;;;    (prep in)
;;;    (noun-phrase
;;;     (simple-noun-phrase (article the) (noun class))
;;;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb-phrase
;;;    (verb lectures)
;;;    (prep-phrase
;;;     (prep to)
;;;     (noun-phrase
;;;      (simple-noun-phrase (article the) (noun student))
;;;      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
;;;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb lectures)
;;;   (prep-phrase
;;;    (prep to)
;;;    (noun-phrase
;;;     (noun-phrase
;;;      (simple-noun-phrase (article the) (noun student))
;;;      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb lectures)
;;;   (prep-phrase
;;;    (prep to)
;;;    (noun-phrase
;;;     (simple-noun-phrase (article the) (noun student))
;;;     (prep-phrase
;;;      (prep in)
;;;      (noun-phrase
;;;       (simple-noun-phrase (article the) (noun class))
;;;       (prep-phrase
;;;        (prep with)
;;;        (simple-noun-phrase (article the) (noun cat)))))))))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (parse '(the professor lectures to the student in the class with the cat))
