#lang racket

;; Exercise 4.23
;;

;;  Alyssa’s analyze-sequence defers the analysis until runtime and at runtime it will 
;;  execute the analysis procedures one after the other. 
;;  Consider how her procedure works when extra expressions are added to the sequence:
;;    (e1)
;;    (lambda (env)
;;      (execute-sequence ((analyze e1)) env))
;; 
;;    (e1 e2)
;;    (lambda (env)
;;      (execute-sequence ((analyze e1) (analyze e2)) env))
;; 
;;    (p1 p2 p3)
;;    (lambda (env)
;;      (execute-sequence ((analyze e1) (analyze e2) (analyze e3)) env))
;;  
;;  
;;
;;  In addition to the work being done in evaluating (analyze e1)... at runtime there is also
;;  the overhead of evaluating the execute-sequence procedure including the predicates in the
;;  cond clause and recursive calls iterating through the sequence. 
;;  The version from the text builds up a more complex single expression at analysis time 
;;  to be evaluated at runtime, without any overhead.
;;    (e1)
;;    (analyse e1)
;; 
;;    (e1 e2)
;;    (lambda (env)
;;      ((analyze e1) env)
;;      ((analyze e2) env))
;;    
;;    (e1 e2 e3)
;;    (lambda (env)
;;      ((lambda (env)
;;         ((analyze e1) env)
;;         ((analyze e2) env))
;;       env)
;;      ((analyze e3) env))


;;; I think the Alyssa's version is less efficient than the text's when there are function calls. In the text's version, the analyze-sequence will return a lambda procedure like

;;;  (lambda (env) 
;;;      ((lambda (env) (proc1 env) (proc2 env)) 
;;;       env) 
;;;      (proc3 env)) 
;;; It's already sequentialized. When it is applied to a env argument, it will apply all the procs to the env.

;;; In the Alyssa's version, the result would be like

;;;  (lambda (env (executive-sequence procs env))) 
;;; The sequential procedure is inside of the executive-sequence, which means there will be extra work whenever there is a function call.

