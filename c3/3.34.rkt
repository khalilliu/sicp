#lang racket


;;  The constraints of multiplier are written such that 2 of the 3 values m1, m2 and product 
;;  are expected in order to work correctly. 
;;  squarer will work when a value is set for a  - the constraints of multiplier are met because
;;  both m1 and m2 have a value - and so product will also have a value. 
;;  squarer should also work when only the value of b is set, but it won't because multiplier 
;;  does not have enough values to fulfil its constraints as only product has a value.
