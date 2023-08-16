#lang racket

(require "lib/amb-eval.rkt")

;; Exercise 4.47

(interpret '(define (require p) (if (not p) (amb))))

(interpret '(define nouns '(noun student professor cat class)))
(interpret '(define verbs '(verb studies lectures eats sleeps)))
(interpret '(define articles '(article the a)))
(interpret '(define prepositions '(prep for to in by with)))
(interpret '(define (displayln l) (display l) (newline)))

(interpret '(define (parse-sentence)
              (displayln "parse-sentence")
              (list 'sentence
                    (parse-noun-phrase)
                    (parse-verb-phrase))))

(interpret '(define (parse-noun-phrase)
              (displayln "parse-sentence")
              (define (maybe-extend noun-phrase)
                (displayln "maybe-extend")
                (amb noun-phrase
                     (maybe-extend (list 'noun-phrase
                                         noun-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-noun-phrase))))

(interpret '(define (parse-simple-noun-phrase)
              (displayln "parse-simple-noun-phrase")
              (list 'simple-noun-phrase
                    (parse-word articles)
                    (parse-word nouns))))

;; Change parse-verb-phrase  
;(interpret '(define (parse-verb-phrase)
;              (define (maybe-extend verb-phrase)
;                (amb verb-phrase
;                     (maybe-extend (list 'verb-phrase
;                                         verb-phrase
;                                         (parse-prepositional-phrase)))))
;              (maybe-extend (parse-word verbs))))



(interpret '(define (parse-verb-phrase)
               (displayln "parse-verb-phrase")
               (amb (parse-word verbs)
                    (list 'verb-phrase
                          (parse-verb-phrase)
                          (parse-prepositional-phrase)))))
           
(interpret '(define (parse-prepositional-phrase)
              (displayln "parse-prepositional-phrase")
              (list 'prep-phrase
                    (parse-word prepositions)
                    (parse-noun-phrase))))

(interpret '(define (parse-word word-list)
              (displayln "parse-word")
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

; This causes an infinite loop when there is a simple verb phrase or
;; when there are no more valid sentences/when the amb is exhausted

;;; ;;; Amb-Eval input:
;;; (parse '(the professor lectures to the cat)) 

;;; ;;; Starting a new problem parse-sentence
;;; parse-sentence
;;; parse-simple-noun-phrase
;;; parse-word
;;; parse-word
;;; maybe-extend
;;; parse-verb-phrase
;;; parse-word
;;; parse-verb-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-sentence
;;; parse-simple-noun-phrase
;;; parse-word
;;; parse-word
;;; maybe-extend

;;; ;;; Amb-Eval value:
;;; (sentence
;;;  (simple-noun-phrase (article the) (noun professor))
;;;  (verb-phrase
;;;   (verb lectures)
;;;   (prep-phrase (prep to) (simple-noun-phrase (article the) (noun cat)))))

;;; ;;; Amb-Eval input:

;;; try-again
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-verb-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-sentence
;;; parse-simple-noun-phrase
;;; parse-word
;;; parse-word
;;; maybe-extend
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-verb-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-sentence
;;; parse-simple-noun-phrase
;;; parse-word
;;; parse-word
;;; maybe-extend
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; parse-verb-phrase
;;; parse-word
;;; parse-prepositional-phrase
;;; parse-word
;;; ...
;;; ..