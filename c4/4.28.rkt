
;;;  for example: 
;;;  (define (g x) (+ x 1)) 
;;;  (define (f g x) (g x)) 
  
;;;  when call (f g 10), 
;;;  if don't use actual-value which will call force-it, 
;;;  g will be passed as parameter which will be delayed, t
;;;  hen g is a thunk, can't be used as function to call 10. 