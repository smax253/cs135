#lang eopl

;; Implemented by Max Shi, CS 135-B, 2/19/2019
;; I pledge my honor that I have abided by the Stevens Honor System

;; This lab is an introduction to using lists in Scheme

;; All lists in Scheme are linked lists, meaning lists
;; are either null or a pair of a head and tail.

;; Scheme has the tick operator ' also called "quote"
;; The tick operator is used to tell the interpreter to treat a symbol
;; as a symbol rather than evaluating the expression.
;; When you quote a list, it quotes each sub-expression in the list.

;; Building Lists

'()  ;; This is how you write an empty list

;; To build a list of multiple values, use the "list" function

;; (list 1 2 3)
;; (list 'a 'b)

;; Define a function "name" that given a first and last name, returns a list
;; with the first and last names.

;; Type signature: (name string string) -> string-list
(define (name first last)
  (list first last))


;; To check if a list is empty, use the "null?" function.
;; Type signature: (null? list) -> boolean

;; To check how many elements are in a list, use "length"
;; Type signature: (length list) -> integer 

;; To reverse the order of elements in a list, use "reverse"
;; Type signature: (reverse list) -> list

;; To add one element to the beginning of a list, we use the "cons" function
;; Type signature: (cons element list) -> list

;; To combine two lists, use the "append" function
;; Type signature: (append list list) -> list

;; To get the first element of a list, use the function "car"
;; Type signature: (car list) -> element

;; To get everything but the first element in the list, use the "cdr" function
;; Type signature (cdr list) -> list

;; NOTE: car and cdr throw exceptions when handed empty lists

;; NOTE: '(a b c d) is the same as (list 'a 'b 'c 'd)

;; Define a function lastName that takes a list containing
;; '(first-name last-name) and returns last-name

(define (lastName name)
  (car(cdr name)))


;; In Scheme you can have lists of lists, like the following:
(define student
  '((IDnumber Degree)
    (LastName FirstName)
    (day month year)
    (class-year ((major) (minor)) GPA)
    ((number street apt) (city state zip))
    (class1 class2 ... classN)))

;; Based on the student template, complete the following definitions
;; using car and cdr

;; Example:
(define (birthday student)
  (car (cdr (cdr student))))
;; Since the birthday is the third element in the student list, we can access it
;; by dropping the first two elements (by using cdr twice) and using car to get the
;; first remaining element.

;; NOTE: Racket has extra functions for shorthands of nesting car and cdr, so
;; birthday could also be written (caddr student)

;; Now define:

;; Returns the 'IDnumber field
(define (IDnumber student)
  (car(car student)))

;; Returns the 'state field
(define (state student)
  (car(cdr(car(cdr(car(cdr( cdr( cdr(cdr student))))))))))

;; Returns the 'GPA field
(define (GPA student)
  (car(cdr(cdr(car(cdr(cdr(cdr student))))))))

;; Returns '((number street apt) (city state zip))
(define (address student)
  (car(cdr(cdr(cdr(cdr student))))))


;; Define a function pig-latin using car, cdr, cons, and append
;; It follows the rules of Pig Latin, so
;; (pig-latin '(h a p p y)) => '(a p p y h a y)
;; (pig-latin '(b i r t h d a y)) => '(i r t h d a y b a y)
;; Type signature (pig-latin symbol-list) -> symbol-list

;; You can use this variable "ay" instead of '(a y) in your definition

(define ay '(a y))

(define (pig-latin word)
  (append (cdr word)(append (list (car word)) ay)))


;; Define a function (yoda 3-words) that takes a list of three words
;; and returns it in Yoda-speak (i.e. (w1 w2 w3) becomes (w3 w1 w2))
;; using the functions you learned above.

;; (yoda '(I am Groot)) => '(Groot I am)
;; (yoda '(I love racket)) => '(racket I love)
;; (yoda '(Naval Engineer Mat)) => '(Mat Naval Engineer)
;; (yoda '(Sarvani is short)) => '(short Sarvani is)

(define (yoda 3-words)
  (append(list (car(reverse 3-words)))(reverse(cdr(reverse 3-words)))))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures
