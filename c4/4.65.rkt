 #lang racket
;; person>? needs to be a primitive in base-eval
;; so we copy base-eval and query-eval into question specific subfolders
(require "lib/query-eval.rkt")

;; Exercise 4.65

; the wheel rule
'(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(pretty-display (run-query '(wheel ?who)))
;;; ((wheel (Warbucks Oliver))
;;;  (wheel (Warbucks Oliver))
;;;  (wheel (Bitdiddle Ben))
;;;  (wheel (Warbucks Oliver))
;;;  (wheel (Warbucks Oliver)))