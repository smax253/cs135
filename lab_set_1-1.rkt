#lang eopl

;; Implemented by Max Shi
;; 3/4/19 CS 135 B
;; I pledge my honor that I have abided by the Stevens Honor System

;; element?, intersection, set-difference are the only functions that cannot be done in a simple one line function. 

;;list: a grouping of elements (multiples allowed)
;;set: a grouping of unique elements (no multiples)
;;Be careful. Some of these functions take lists and return sets.

;; Define element?
;; Given item, and list-of-items, return #t if item is in list-of-items,
;; #f otherwise

;; Examples:
;; (element? 0 '()) => #f
;; (element? 8 '(7 8 9)) => #t
;; (element? 7 '(1 2 3 4)) => #f
;; (element? 'a '(the man saw a dog)) => #t


;; Type signature: (element? element list) -> boolean

(define (element? item list-of-items)
  (if(null? list-of-items)
     #f
   (if(equal? (car list-of-items) item)
     #t
     (element? item (cdr list-of-items)))))


;; make-set takes a list and removes all duplicates
;; This will be useful for the rest of our functions

;; Type signature: (make-set list) -> set

(define (make-set list-of-items)
  (cond
    [(null? list-of-items)
     '()] ; Empty lists never have duplicates
    [(element? (car list-of-items) (cdr list-of-items))
     (make-set (cdr list-of-items))]
    [else
     (cons (car list-of-items) (make-set (cdr list-of-items)))]))


;; Define union
;; Given two lists, return a set representing the set which
;; contains all of the elements from each list EXACTLY once.

;; NOTE: Order does not matter when checking your work for union

;; Examples:
;; (union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6) 
;; (union '(1 2 3) '(1 2 3)) => '(1 2 3)
;; (union '(1 1 1) '()) => '(1)

;; Type signature: (union list list) -> set

(define (union listA listB)
  (make-set (append listA listB)))


;; Define intersection
;; Given two lists A and B, return the set representing the set which
;; contains the elements in A and in B.

;; NOTE: Order will not matter when checking your work for intersection

;; Examples:
;; (intersection '(1 2 3 4) '(2 4 5)) => '(2 4)
;; (intersection '(s a m u e l) '(k r a u s)) => '(a u s)
;; (intersection '(r i p) '(l i e b)) => '(i)
;; (intersection '(a a a) '(a a a)) => '(a)

;; Type signature: (intersection list list) -> set

(define (intersection listA listB)
  (if (null? listA)
      '()
      (if (element? (car listA) listB)
          (make-set(cons(car listA)(intersection (cdr listA)listB)))
          (intersection(cdr listA)listB)
      )))


;; Define subset?
;; Takes two sets returns whether the first is the subset of the second.
;; (i.e., every element in the first is also in the second set)

;; Examples:
;; (subset? '() '()) => #t
;; (subset? '(1 2 3) '(1 2 3 4 5)) => #t
;; (subset? '(115 284 385) '(115 146 284 135 385 392)) => #t
;; (subset? '(-2 0 2) '(-1 1 3)) => #f
;; (subset? '(-1 1 2) '(-1 1 3 5 7)) => #f

;; Type signature: (subset? set set) -> boolean

(define (subset? setA setB)
  (if (null? setA)
      #t
      (if (element? (car setA) setB)
          (subset? (cdr setA) setB)
          #f
  )))


;; Define set-equal?
;; Determines whether two sets are equivalent (i.e. A = B => every element
;; in A is in B and every element in B is in A)
;; NOTE: order does not matter, so you cannot simply use (equal? A B).
;; NOTE: For the simplest solution, this can be a one-liner

;; Examples:
;; (set-equal? '() '()) => #t
;; (set-equal? '(a b c) '(a b c)) => #t
;; (set-equal? '(1 2 3 4) '(4 3 1 2)) => #t
;; (set-equal? '(1 2 3) '(1 2 4)) => #f

;; Type signature: (set-equal? set set) -> boolean

(define (set-equal? setA setB)
  (and (subset? setA setB)(subset? setB setA)))


;; Define set-difference
;; The set difference A - B is the set of the list of all elements of A
;; which are not in list B.

;; Examples:
;; (set-difference '(1 2 3) '(2 3 4)) => '(1)
;; (set-difference '(1 2 3) '(1 2 3)) => '()
;; (set-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
;; (set-difference '() '(1 2 3))      => '()
;; (set-difference '(1 1 2 3 3) '())  => '(1 2 3)

;; Type signature: (set-difference list list) -> set

(define (set-difference listA listB)
  (if (null? listA)
      '()
      (if(element? (car listA)listB)
         (set-difference(cdr listA)listB)
         (make-set (cons (car listA)(set-difference (cdr listA) listB))))
         ))


;; Define sym-diff
;; The symmetric difference of two lists (A, B) is the union of
;; A - B and B - A. (It is the set equivalent of the logical operator
;; "xor".)

;; Examples:
;; (sym-diff '(1 2 3) '(3 4 5)) => '(1 2 4 5)
;; (sym-diff '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (sym-diff '(1 2 3) '(1 2 3)) => '()
;; (sym-diff '(1 2) '(1 2 3 4)) => '(3 4)
;; (sym-diff '(1 1 1) '()) => '(1)

;; Type signature: (sym-diff list list) -> set

(define (sym-diff listA listB)
  (union (set-difference listA listB)(set-difference listB listA)))



;; Define cardinality
;; the cardinality of a list |A| is the number of unique elements in
;; a collection, or the length of the set.

;; Examples:
;; (cardinality '(1 2 3))    => 3
;; (cardinality '(1 1 2 3 3) => 3
;; (cardinality '(1 1 1 1 1) => 1
;; (cardinality '() )        => 0

;; Type signature: (cardinality list) -> int

(define (cardinality lst)
  (length(make-set lst)))


;; Define disjoint
;; Two sets are disjoint if they don't have any elements in common.
;; I.e., their intersection is empty.

;; Examples:
;; (disjoint? '(1 2 3) '()) => #t
;; (disjoint? '(1 2 3) '(1) => #f
;; (disjoint? '(1 2 3) '(4 5 6) => #t

;; Type signature: (disjoint? set set) -> boolean

(define (disjoint? setA setB)
  (null? (intersection setA setB)))


;; Define superset?
;; A is a superset B if every element in B is an element of A.
;; i.e. (A >= B) <==> (B <= A)

;; Examples:
;; (superset? '() '()) => #t
;; (superset? '(1 2 3 4 5) '(1 2 3)) => #t
;; (superset? '(-1 1 3) '(-2 0 2)) => #f

;; Type signature: (superset? set set) -> boolean

(define (superset? setA setB)
  (subset? setB setA))


;; Define insert
;; Takes a list and an element and returns the set with the element
;; added to the set (NOTE: if the element is already present, it just
;; returns the original set)

;; Examples:
;; (insert 0 '(1 2 3)) => '(0 1 2 3)  [or '(1 2 3 0) etc.]
;; (insert 1 '(1 2 3)) => '(1 2 3)
;; (insert 0 '(0 0 0)) => '(0)

;; Type signature: (insert element list) => set

(define (insert element lst)
  (make-set (cons element lst)))


;; Define remove
;; Takes an element and a set and returns the set without that element.
;; NOTE: if the element is not initially in the set, it should return the
;; equivalent of the original set rather than raising an exception.

;; (remove 2 '(1 2 3)) => '(1 3)
;; (remove 3 '(3))     => '()
;; (remove 4 '(1 2 3)) => '(1 2 3)

;; Type signature: (remove element set) => set

(define (remove element set)
  (set-difference set (list element)))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures
