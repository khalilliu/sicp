#lang racket

(require "lib/stream.rkt")

(define factorials 
  (cons-stream 1 
               (mul-streams factorials (stream-cdr integers))))

(show-stream factorials 10)

