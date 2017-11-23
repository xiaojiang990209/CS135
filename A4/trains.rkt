;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang (20714726)
;; CS135 Fall 2017
;; Assignment 04, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a04lib.rkt")


;; Useful constants for examples and testing
(define proper-train (string->train "LLBTPPC"))
(define improper-train (string->train "CPPTLCB"))
(define train-not-end-with-caboose (string->train "LBTP"))
(define test-train-with-3-removed
  (cons (make-unit 'L 2)
                    (cons (make-unit 'C 5) empty)))
(define test-train-with-2-removed
  (cons (make-unit 'B 3)
                    (cons (make-unit 'C 5) empty)))
(define train-with-only-caboose (string->train "C"))
(define train-with-only-locomotive (string->train "L"))
(define train-with-only-car (string->train "T"))


;; The unit structure is defined in a04lib.rkt.  The require
;; statement, above, is all that's needed to have it take
;; effect here.  The following comment is here just so the
;; type definitions that follow make sense.

;; (define-struct unit (type serial))

;; -------- Q4a --------------
;; A Unit-Type is one of:
;; * 'L, 'B, 'T, 'P, 'C

;; A Unit is a (make-unit Sym Nat)

;; A Train is one of:
;; * empty
;; * (cons Unit Train)


;; -------- Q4b --------------

;; string->train works by first converting the string units to a list of
;;   characters using string->list. Then the list of characters and the
;;   the default serial number are passed into loc->train, which constructs
;;   a Train by constructing the first unit and then concatenating it with
;;   the previously generated Train from the recursive call of loc-train


;; -------- Q4c --------------
;; (headed-by? train unit_type) produces true if the first unit of
;;   of train has the given unit_type, and false otherwise
;; headed-by?: Train Unit-Type -> Bool
;; Example
(check-expect (headed-by? test-train 'L) true)

(define (headed-by? train unit_type)
  (cond [(empty? train) false]
        [(symbol=? unit_type
                          (unit-type (first train))) true]
        [else false]))

;; Tests
(check-expect (headed-by? empty 'L) false)
(check-expect (headed-by? test-train 'T) false)
(check-expect (headed-by? improper-train 'C) true)


;; -------- Q4d -------------
;; (ends-with-caboose? train) produces true if and only if there is
;;   exactly one caboose and it is the last unit of train
;; ends-with-caboose?: Train -> Bool
;; Example
(check-expect (ends-with-caboose? test-train) true)

(define (ends-with-caboose? train)
  (cond [(empty? train) false]
        [(empty? (rest train))
         (cond [(symbol=? 'C (unit-type (first train))) true]
               [else false])]
        [(symbol=? 'C (unit-type (first train))) false]
        [else (ends-with-caboose? (rest train))]))

;; Tests
(check-expect (ends-with-caboose? empty) false)
(check-expect (ends-with-caboose? proper-train) true)
(check-expect (ends-with-caboose? improper-train) false)
(check-expect (ends-with-caboose? train-not-end-with-caboose) false)
(check-expect (ends-with-caboose? train-with-only-caboose) true)


;; -------- Q4e ------------
;; (remove-unit train serial-number) produces a Train identical
;;   to t except that the unit with serial number s is removed
;; remove-unit: Train Nat -> Train
;; Example
(check-expect (remove-unit test-train 3) test-train-with-3-removed)

(define (remove-unit train serial-number)
  (cond [(empty? train) empty]
        [(= (unit-serial (first train)) serial-number)
                (remove-unit (rest train) serial-number)]
        [else
         (cons (first train)
               (remove-unit (rest train) serial-number))]))

;; Tests
(check-expect (remove-unit empty 3) empty)
(check-expect (remove-unit test-train 2) test-train-with-2-removed)


;; -------- Q4f -------------
;; (valid-train-unit? unit1 unit2) produces true if unit2 is allowed
;;   to follow unit1, and false otherwise
;; valid-train-unit?: Unit Unit -> Bool
;; Example
(check-expect (valid-train-unit? (make-unit 'L 1) (make-unit 'B 2)) true)

(define (valid-train-unit? unit1 unit2)
  (or (symbol=? 'L (unit-type unit1))
      (symbol=? 'C (unit-type unit2))
      (symbol=? (unit-type unit1) (unit-type unit2))
      (and (or (symbol=? 'B (unit-type unit1))
               (symbol=? 'T (unit-type unit1))
               (symbol=? 'P (unit-type unit1)))
           (or (symbol=? 'C (unit-type unit2))
               (symbol=? 'B (unit-type unit2))
               (symbol=? 'T (unit-type unit2))
               (symbol=? 'P (unit-type unit2))))))


;; (proper-train? train) produces true if train is a proper train
;;   and false otherwise.
;; proper-train?: Train -> Bool
;; Example
(check-expect (proper-train? test-train) true)

(define (proper-train? train)
  (cond [(empty? train) true]
        [(empty? (rest train)) true]
        [else
         (and (proper-train? (rest train))
              (valid-train-unit? (first train) (first (rest train))))]))

;; Tests:
(check-expect (proper-train? empty) true)
(check-expect (proper-train? train-with-only-caboose) true)
(check-expect (proper-train? train-with-only-locomotive) true)
(check-expect (proper-train? train-with-only-car) true)
(check-expect (proper-train? proper-train) true)
(check-expect (proper-train? improper-train) false)