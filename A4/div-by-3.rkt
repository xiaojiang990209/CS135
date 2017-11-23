;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 04, Problem 3
;; *******************************************************
;;


;; (a) Data definition
;; A Nat3 is one of:
;; * 0
;; * 1
;; * 2
;; * (+ 3 Nat3)


;; (b) Template for Nat3
;; nat3-template: Nat3 -> Any
(define (nat3-template n)
  (cond [(zero? n) ...]
        [(= n 1) ...]
        [(= n 2) ...]
        [else ( ... (nat3-template (- n 3)) ... )]))


;; (c)
;; (div-by-3? nat3) consumes nat3 and produces true if it is
;;   divisible by 3, and false otherwise
;; div-by-3?: Nat3 -> Bool
;; Example
(check-expect (div-by-3? 998) false)

(define (div-by-3? nat3)
  (cond [(zero? nat3) true]
        [(= nat3 1) false]
        [(= nat3 2) false]
        [else (div-by-3? (- nat3 3))]))

;; Tests
(check-expect (div-by-3? 12345) true)
(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 1) false)
(check-expect (div-by-3? 2) false)
(check-expect (div-by-3? 3) true)
(check-expect (div-by-3? 4) false)