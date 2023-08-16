#lang racket

(require "lib/amb-eval.rkt")

;; Exercise 4.45 version a
(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items)))))

(interpret '(define (remove-item item items)
  (cond [(null? items) null]
        [(equal? item (car items)) (cdr items)]
        [else (cons (car items) (remove-item item (cdr items)))]))
)

(interpret '(define (random-element-of items)
  (require (not (null? items)))
  (let ((item (list-ref items (random (length items)))))
        (amb item (random-element-of (remove-item item items)))))
)
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
              (let ((found-word (random-element-of (cdr word-list))))
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
;;; (parse '(the professor eats))

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun cat)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun cat)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun cat)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun cat)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun class)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun class)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun class)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article the) (noun class)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun student)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun student)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun student)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun student)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun professor)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun professor)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun professor)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun professor)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun class)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun class)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun class)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun class)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun cat)) (verb lectures))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun cat)) (verb eats))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun cat)) (verb studies))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (sentence (simple-noun-phrase (article a) (noun cat)) (verb sleeps))


;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (parse '(the professor eats))


;;; ;;; Amb-Eval input:
