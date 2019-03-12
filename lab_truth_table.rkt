#lang eopl

;; Written by Max Shi
;; 2/5/2019 I pledge my honor that I have abided by the Stevens Honor System

;; Some basic logic operators, defined in terms of Scheme's built-in 
;; and,or,not 

;; Remember that Scheme uses prefix notation, so "x ^ y" is written "(and x y)"


;; Define (nand p q) which follows this truth table:
;; ((#t #t) #f)
;; ((#t #f) #t)
;; ((#f #t) #t)
;; ((#f #f) #t)


;; Type signature: (nand boolean boolean) -> boolean

(define (nand p q)
  (not(and p q)))


;; Define if->then, it has the truth table
;; ((#t #t) #t)
;; ((#t #f) #f)
;; ((#f #t) #t)
;; ((#f #f) #t)

;; Type signature: (if->then boolean boolean) -> boolean

(define (if->then p q)
  (or (not p) q))


;; Define (xor p q) exclusive or
;; ((#t #t) #f)
;; ((#t #f) #t)
;; ((#f #t) #t)
;; ((#f #f) #f)

;; Type signature: (xor boolean boolean) -> boolean

(define (xor p q)
  (and (or p q) (or (not p) (not q))))


;; Define (nor p q)
;; ((#t #t) #f)
;; ((#t #f) #f)
;; ((#f #t) #f)
;; ((#f #f) #t)

;; Type signature: (nor boolean boolean) -> boolean
(define (nor p q)
  (and (not p)(not q)))


;; Define (3majority p q r)
;; Create a function that returns #t when a majority of its three
;; arguments are #t
;; 
;; ((#t #t #t) #t)
;; ((#t #t #f) #t)
;; ((#t #f #t) #t)
;; ((#t #f #f) #f)
;; ((#f #t #t) #t)
;; ((#f #t #f) #f)
;; ((#f #f #t) #f)
;; ((#f #f #f) #f)

;; Type signature: (3majority boolean boolean boolean) -> boolean
(define (3majority p q r)
  (or (or (and p q) (and p r)) (and q r)))


;; Define (isosceles p q r)
;; Create a function that returns #t when exactly two of its
;; arguments are #t

;; ((#t #t #t) #f)
;; ((#t #t #f) #t)
;; ((#t #f #t) #t)
;; ((#t #f #f) #f)
;; ((#f #t #t) #t)
;; ((#f #t #f) #f)
;; ((#f #f #t) #f)
;; ((#f #f #f) #f)

(define (isosceles p q r)
  (or(or(and (and p q)(not r))(and (and q r) (not p))) (and (and p r)(not q))))


;; Define (commutative? operator)
;; Returns #t iff the operator is commutative
;; i.e., in all cases (operator p q) is the same as (operator q p)
;; E.g. (commutative? nand) is #t since (nand p q) = (nand q p) in
;; all cases
;; NOTE: this will not work with 'and' and 'or'

;; Type signature: (commutative? operator) -> boolean
;; Where operator's type signature is (operator boolean boolean) -> boolean
(define (commutative? operator)
  (and(operator #t #f)(operator #f #t)))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures
