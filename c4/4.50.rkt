#lang racket/base
(require "lib/ramb-eval.rkt")

(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items)))))

(interpret '(define (random-element-of items)
              (require (not (null? items)))
              (ramb (car items) (random-element-of (cdr items)))))

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
