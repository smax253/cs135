#lang eopl

;; Implemented by Max Shi
;; CS 135 B April 9 2019
;; I pledge my honor that I have abided by the Stevens Honor System

;; From this lab on all functions can be written with either recursion, tail-recursion,
;; map, filter, reduce, or a combination of them.
;; modulo is the call for %
;; quotient is the call for integer division


;; Define convert10
;; Given an integer (in base 10) and a desired base convert the integer into the other base
;; storing the new number as a list. The number should correctly read from left to right.
;; The easiest way to do this will be using the quotient-remainder theorem.
;; To do this take the mod of the number with the new base and then divide the number by the base
;; Once the number is 0 you should have found the number in the new base backwards.

;; Examples:
;; (convert10 100 8) -> '(1 4 4)
;; (convert10 215 3) -> '(2 1 2 2 2)
;; (convert10 5736 19) -> '(15 16 17)

;; Type signature: (convert10 int int) -> int-list
(define (convert10-helper number base)
  (if (zero? number)
      '()
      (cons (modulo number base) (convert10-helper (quotient number base) base))
  ))

(define (convert10 number base)
  (reverse(convert10-helper number base)))

;; Define base10
;; Given a base and a list that represents a number in that base convert it back into an int (base 10)
;; There are two main ways to do this. First you can calculate the multiplier for each number by looking
;; at the length of the list and using (expt base power). The other method would be to reverse the list
;; and store the multiplier at each step which would be the tail recursive way of doing it.

;; Examples:
;; (base10 3 '(1 2 1 2)) -> 50
;; (base10 7 '(4 5 6 0)) -> 1659
;; (base10 10 '(1 2 3 4 5)) -> 12345

;; Type signature: (base10 int int-list) -> int


(define (base10 oldbase numlist)
  (if (null? numlist)
      0
      (+ (* (car numlist) (expt oldbase (- (length numlist) 1))) (base10 oldbase (cdr numlist)))
  ))

;; Define convert
;; Given a list in oldbase form convert a list of the new base.
;; To do this use the functions above. Order should be left to right like before

;; Examples:
;; (convert 7 3 '(1 2 3 4 5)) -> '(1 1 1 1 1 0 0 0)
;; (convert 10 4 '(9 6 3 0)) -> '( 2 1 1 2 1 3 2)
;; (convert 3 15 '(2 1 0 0 1 2)) -> '(2 8 2)

;; Type signature: (convert int int int-list) -> int-list

(define (convert oldbase newbase intlist)
  (convert10 (base10 oldbase intlist) newbase))

;; Define geo-prog
;; This computes the geometric progression starting at cur_num and going n times.
;; This is a sequence where each successive number is different by a factor of ratio of the
;; previous number. Once again order matters and should go first number to last number.

;; Examples:
;; (geo-prog 1 10 5) -> '(1 10 100 1000 10000)
;; (geo-prog 1 2 11) -> '(1 2 4 8 16 32 64 128 256 512 1024)
;; (geo-prog 7 -3 5) -> '(7 -21 63 -189 567)

;; Type sinature: (geo-prog num num int) -> num-list
(define (geo-prog cur_num ratio n)
  (if (zero? n)
      '()
      (cons cur_num (map(lambda (x) (* ratio x)) (geo-prog cur_num ratio (- n 1))))))