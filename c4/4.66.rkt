 #lang racket

(require "lib/query-eval.rkt")

;; Exercise 4.66

(run-query '(sum ?amount 
                   (and (wheel ?who)
                        (salary ?who ?amount))))


;(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
;(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
;(and (wheel (Bitdiddle Ben)) (salary (Bitdiddle Ben) 60000))
;(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
;(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
