#lang eopl

;; Max Shi
;; CS 135 B 3/12/19
;; I pledge my honor that I have abided by the Stevens Honor System

;; At the bottom of the file will be a lot of functions that will be helpful for this lab.
;; This lab is a review of relations.
;; A relation is a set of unique integer tuples

;; Define id
;; id should take an integer and create the set id.
;; This means it should create the pairs (1 1) (2 2) ... (n n)
;; Order does not matter

;; Examples:
;; (id 1) -> '((1 1))
;; (id 5) -> '((1 1) (2 2) (3 3) (4 4) (5 5))

;; Type Signature: (id int) -> relation

(define (id n)
    (if (zero? n)
        '()
        (append (list (list n n)) (id (- n 1)))
        )
  )

;; Define reflexive?
;; reflexive? takes a relation and max int size and returns if the relation is reflexive.
;; A relation is reflexive iff it contains the the id relation.
;; This can be done easily using id and one of the functions provided.

;; Examples:
;; (reflexive? '((1 1) (2 2) (3 3)) 3) -> #t
;; (reflexive? '((1 1) (2 2) (3 3) (3 2) (2 3)) 3) -> #t
;; (reflexive? '((1 1) (2 2) (3 3)) 4) -> #f

;; Type Signature: (reflexive? relation int) -> boolean

(define (reflexive? relation n)  
    (set-equal? (intersection (id n) relation) (id n)))

;; Define reflexive-closure
;; Reflexive closure adds the id relation to a given relation
;; This can be done easily using id and one of the functions provided.
;; Order does not matter

;; Examples:
;; (reflexive-closure '() 3) -> '((1 1) (2 2) (3 3))
;; (reflexive-closure '((3 2) (2 3)) 3) -> '((1 1) (2 2) (3 3) (3 2) (2 3))
;; (reflexive-closure '((1 1) (2 2) (3 3)) 4) -> '((1 1) (2 2) (3 3) (4 4))

;; Type Signature: (reflexive-closure relation int) -> relation

(define (reflexive-closure relation n)
    (union (id n) relation))

;; Define flip-pairs
;; flip-pairs takes a relation and changes all (x y) pairs into (y x)
;; This can be done with simple recursion and using the reverse keyword
;; Order does not matter in the set.


;; Examples:
;; (flip-pairs '((1 2) (3 2) (4 5))) -> '((2 1) (2 3) (5 4))
;; (flip-pairs '((1 1) (1 2) (1 3))) -> '((1 1) (2 1) (3 1))

;; Type Signature: (flip-pairs relation) -> relation

(define (flip-pairs relation)
    (if (null? relation)
        '()
        (append(list(reverse (car relation))) (flip-pairs (cdr relation)))
        )
  )

;; Define symmetric?
;; symmetric? takes a relation and returns if the relation is symmetric.
;; A relation is symmetric iff for every (x y) pair it has (y x)
;; This can be done easily using flip-pairs and one of the functions provided.

;; Examples:
;; (symmetric? '((1 1) (2 1) (1 2))) -> #t
;; (symmetric? '((1 1) (2 4) (3 7) (3 5) (5 3))) -> #f
;; (symmetric? '((2 4) (4 3) (3 4) (4 2))) -> #t

;; Type Signature: (reflexive? relation int) -> boolean

(define (symmetric? relation)
   (set-equal? (intersection (flip-pairs relation) relation) relation))

;; Define symmetric-closure
;; Symmetric closure adds the symmetric pairs to a given relation
;; So for every (x y) pair it adds (y x) iff (y x) isn't in the set already.
;; This can be done easily using flip-pairs and one of the functions provided.
;; Order does not matter

;; Examples:
;; (symmetric-closure '()) -> '()
;; (symmetric-closure '((3 2) (2 3))) -> '((3 2) (2 3))
;; (symmetric-closure '((1 2) (2 7) (3 4))) -> '((1 2) (2 7) (3 4) (2 1) (7 2) (4 3))

;; Type Signature: (reflexive-closure relation int) -> relation

(define (symmetric-closure relation)
  (union (flip-pairs relation) relation))

;; Define related-to
;; related to takes an element and relation and returns the list of elements that the given element is related to
;; On a given (x y) pair an element is related to y iff the element is the same as x

;; Examples:
;; (related-to 3 '((3 3) (3 4) (3 5))) -> '(3 4 5)
;; (related-to 1 '((1 2) (2 3) (3 4) (4 5))) -> '(2)
;; (related-to 2 '((1 3) (3 5) (4 6) (8 2))) -> '()
(define (related-to element relation)
   (if (null? relation)
       '()
       (if(equal? (car(car relation)) element)
          (cons (car(cdr (car relation))) (related-to element (cdr relation)))
          (related-to element (cdr relation))
       )))
          

;;_____________________________________________________________________
(define (element? item list-of-items)
  (if (null? list-of-items)                  ;Is our "set" empty?
      #f                                     ;If empty, not an element!
      (if (equal? item (car list-of-items))  ;Is our item first in list?
          #t                                 ;Yes?  Then it's an element!
          (element? item (cdr list-of-items)))));No? Check the rest.

(define (make-set list-of-items)
  (if (null? list-of-items) ;An empty list can have no duplicates,
      '()                   ;so just return an empty list.
      (if (element? (car list-of-items) (cdr list-of-items))
          (make-set (cdr list-of-items))
          (cons (car list-of-items) (make-set (cdr list-of-items))))))
         
(define (union setA setB)
  (make-set (append setA setB))) 

(define (intersection setA setB)
  (make-set (Intersection (make-set setA) (make-set setB))))

(define (Intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (cons (car setA) (intersection (cdr setA) setB))
          (intersection (cdr setA) setB))))

(define (subset? setA setB)
  (if (null? setA)
      #t
      (if (element? (car setA) setB)
          (subset? (cdr setA)  setB)
          #f)))

(define (set-equal? setA setB)
   (and (subset? setA setB) (subset? setB setA)))

(define (proper-subset? setA setB)
  (and (subset? setA setB) (not (set-equal? setA setB))))

(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))

(define (Set-Difference setA setB)
  (if (null? setA)
      '()
      (if (element? (car setA) setB)
          (Set-Difference (cdr setA) setB)
          (cons (car setA) (Set-Difference (cdr setA) setB)))))

(define (sym-diff setA setB)
  (union (set-difference setA setB) (set-difference setB setA)))

(define (cardinality set)
  (length (make-set set)))

(define (disjoint? setA setB)
  (null? (intersection setA setB)))

(define (superset? setA setB)
  (subset? setB setA))

(define (insert element set)
  (make-set (cons element set)))

(define (remove element set)
  (set-difference set (list element)))