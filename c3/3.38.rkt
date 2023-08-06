#lang sicp

;;If no interleaving is possible the resulting values can be:
;;
;;    45: Peter +10; Paul -20; Mary /2
;;    35: Peter +10; Mary /2; Paul -20
;;    45: Paul -20; Peter +10; Mary /2
;;    50: Paul -20; Mary /2; Peter +10
;;    40: Mary /2; Peter +10; Paul -20
;;    40: Mary /2; Paul -20; Peter +10

