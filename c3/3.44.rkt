;;  Louis is wrong in this simple scenario. 

;;  In Exercise 3.43 we saw the total is preserved and so it is safe to write transfer
;;  in this way – assuming that the procedure doesn’t crash in between the two mutations. 
;;  
;;  The key difference between transfer and exchange is that transfer is 3 steps 
;;  (calculate difference b-a, withdraw a, deposit b) whereas exchange should be 4 steps 
;;  (calculate difference b-a, withdraw a, calculate difference b-a, deposit a).
