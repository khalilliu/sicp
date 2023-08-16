#lang racket

(require "lib/amb-eval.rkt")


;;; That's because function parse-word handles *unparsed* from left to right. 
;;; If evaluation has other order, it will conflict with parse-word. 

;; Exercise 4.46

;
;(parse '(the cat studies with the student)) 
;(set! *unparsed* '(the cat studies with the student))
;(let ((sent (parse-sentence))
;      (parse-sentence) 
;(list 'sentence (parse-noun-phrase) (parse-verb-phrase))))
;(parse-verb-phrase) 
;(maybe-extend (parse-word verbs))
;(parse-word '(verb studies lectures eats sleeps))
;(require (not (null? *unparsed*))) 
;(require (memq (car *unparsed*) (cdr '(verb studies lectures eats sleeps))))
;(cdr '(verb studies lectures eats sleeps)) => '(studies lectures eats sleeps)
;(car *unparsed*) => 'the
;fails
