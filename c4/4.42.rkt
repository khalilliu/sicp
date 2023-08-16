#lang racket

(require "lib/amb.rkt")

;; Exercise 4.42

(define (amb-liars)
  (let ((betty (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5)))
      (require (betty-statement betty kitty))
      (let ((mary (amb 1 2 3 4 5)))
          (require (mary-statement mary betty))
          (require (kitty-statement kitty mary))      
          (let ((joan (amb 1 2 3 4 5))
              (ethel (amb 1 2 3 4 5)))
              (require (joan-statement joan ethel))
              (require (ethel-statement ethel joan))
              (require (distinct? (list betty ethel joan kitty mary)))
              (present-solution (list betty ethel joan kitty mary))))))

(define (liars)
  (define (invalid-solution? permutation)
     (let ((betty (first permutation))
          (ethel (second permutation))
          (joan (third permutation))
          (kitty (fourth permutation))
          (mary (fifth permutation)))
          
      (and (betty-statement betty kitty)
           (mary-statement mary betty)
           (ethel-statement ethel joan)
           (joan-statement joan ethel)
           (kitty-statement kitty mary))))
  (map present-solution
    (filter invalid-solution? 
      (permutations '(1 2 3 4 5))))
)

(define (present-solution solution)
  (map list 
       '(betty ethel joan kitty mary)
       solution))

(define (xor a b)
  (or (and a (not b))
      (and (not a) b)))

; Betty: Kitty was second in the examination. I was only third.
(define (betty-statement betty kitty)
  (xor (= kitty 2) (= betty 3)))

; Ethel: You'll be glad to hear that I was on top. Joan was second.
(define (ethel-statement ethel joan)
  (xor (= ethel 1) (= joan 2)))

; Joan: I was third, and poor old Ethel was bottom
(define (joan-statement joan ethel)
  (xor (= joan 3) (= ethel 5)))

; Kitty: I came out second. Mary was only fourth.
(define (kitty-statement kitty mary)
  (xor (= kitty 2) (= mary 4)))

; Mary: I was fourth. Top place was taken by Betty.
(define (mary-statement mary betty)
  (xor (= mary 4) (= betty 1)))



(define tests 100)

(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) (void))
         (else (amb-liars)
               (next (- x 1))))))


(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) (void))
         (else (liars)
               (next (- x 1))))))


;;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
;;; cpu time: 11 real time: 12 gc time: 0

;;; (((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4)))
;;; cpu time: 2 real time: 2 gc time: 0