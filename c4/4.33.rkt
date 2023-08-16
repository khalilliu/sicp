#lang racket 

(require "lib/lazy-list-eval.rkt")

(interpret '(car '(a b c)))