 #lang racket
(require "lib/query-eval.rkt")

;; Exercise 4.64


;; original rule
;;; '(rule (outranked-by ?staff-person ?boss)
;;;       (or (supervisor ?staff-person ?boss)
;;;           (and (supervisor ?staff-person ?middle-manager)
;;;                (outranked-by ?middle-manager ?boss))))

;; new rule
'(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss) ;;; fault:  ?middle-manager is not bound
               (supervisor ?staff-person ?middle-manager))))


