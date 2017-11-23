;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 04, Bonus
;; *******************************************************
;;


(require "a04lib.rkt")


;; (sum-digits nat) computes the sum of the digits of nat
;; sum-digits: Nat -> Nat
;; Example:
(check-expect (sum-digits 12345) 15)

(define (sum-digits nat)
  (cond [(zero? nat) 0]
        [else
         (+ (last-digit nat) (sum-digits (other-digits nat)))]))


;; (div-by-3-alt? nat) produces true if nat is divisible by 3
;;   and false otherwise
;; div-by-3-alt?: Nat -> Bool
;; Example:
(check-expect (div-by-3-alt? 0) true)
(check-expect (div-by-3-alt? 5) false)

(define (div-by-3-alt? nat)
  (cond [(< nat 3)
         (cond [(zero? nat) true]
               [else false])]
        [else
         (div-by-3-alt? (- (sum-digits nat) 3))]))

;; Tests
(check-expect (div-by-3-alt? 9996) true)
(check-expect (div-by-3-alt? 12345) true)