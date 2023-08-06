#lang racket

(require "lib/stream.rkt")

(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))


(show-stream fibs 10)