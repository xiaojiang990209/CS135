;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 04, Problem 2
;; *******************************************************
;;


;; (sum-positive loi) consumes an integer list loi and produces
;;    the sum of all positive integers in loi
;; sum-positive: (listof Int) -> Nat
;; Example
(check-expect (sum-positive (cons 5 (cons -3 (cons 4 empty)))) 9)

(define (sum-positive loi)
  (cond [(empty? loi) 0]
        [(> (first loi) 0)
         (+ (first loi) (sum-positive (rest loi)))]
        [else (sum-positive (rest loi))]))

;; Tests
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive (cons -5 (cons -8 empty))) 0)


;; (contains? elem list) produce true if elem is in list and
;;    false otherwise
;; contains?: Any (listof Any) -> Bool
;; Example
(check-expect (contains? 'fun
                         (cons 'racket (cons 'is (cons 'fun empty))))
              true)

(define (contains? elem list)
  (cond [(empty? list) false]
        [(equal? elem (first list)) true]
        [(contains? elem (rest list)) true]
        [else false]))

;; Tests
(check-expect (contains? 'boring
                         (cons 'racket (cons 'is (cons 'fun empty))))
              false)
(check-expect (contains? 'boring empty) false)
(check-expect (contains? 'fun
                         (cons 'fun (cons 'is (cons 'racket empty))))
              true)


;; (has-duplicate? list) produces true if any element in list
;;   appears more than once, and false otherwise
;; has-duplicate?: (listof Any) -> Bool
;; Example
(check-expect (has-duplicate? (cons 1 (cons 2 (cons 2 empty)))) true)

(define (has-duplicate? list)
  (cond [(empty? list) false]
        [(contains? (first list) (rest list)) true]
        [(has-duplicate? (rest list)) true]
        [else false]))

;; Tests
(check-expect (has-duplicate? empty) false)
(check-expect (has-duplicate? (cons 1 (cons 2 empty))) false)
(check-expect (has-duplicate? (cons 1 (cons 3 (cons 2 (cons 3 empty)))))
              true)


;; (keep-ints list) consumes list and produces a list that contains
;;    only the integers in the given list, in their original order
;; keep-ints: (listof Any) -> (listof Int)
;; Example
(check-expect (keep-ints (cons 'a (cons 1 (cons "b" (cons 2 empty)))))
              (cons 1 (cons 2 empty)))

(define (keep-ints list)
  (cond [(empty? list) empty]
        [(integer? (first list))
         (cons (first list) (keep-ints (rest list)))]
        [else (keep-ints (rest list))]))

;; Tests
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints (cons 8.5 (cons -6.5 (cons 0.33 empty)))) empty)