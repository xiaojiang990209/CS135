;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname airmiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 2
;; **********************************************
;;


;; (calc-airmile-per-dollar-spent dollar dollar-per-AirMile) calculates
;;    the number of AirMiles received based on how much dollar is
;;    spent and dollar-per-AirMile, which is how many dollars need to be
;;    spent to gain one AirMile
;; calc-airmile-per-dollar-spent: Num Nat -> Nat
;; requires: dollar >= 0
;; Examples:
(check-expect (calc-airmile-per-dollar-spent 70 10) 7)
(check-expect (calc-airmile-per-dollar-spent 60.5 15) 4)

(define (calc-airmile-per-dollar-spent dollar dollar-per-AirMile)
  (quotient (floor dollar) dollar-per-AirMile)) 


;; (calc-airmiles dollar card-type is-sponsor) produces the number of
;;    AirMiles earned for the given purchase based on the dollar
;;    purchased, the card-type and is-sponsor, which denotes if the
;;    store is a sponsor.
;; calc-airmiles: Num Sym Bool -> Nat
;; requires: dollar >= 0
;; Examples:
(check-expect (calc-airmiles 70 'standard false) 3)
(check-expect (calc-airmiles 60 'premium true) 6)

(define (calc-airmiles dollar card-type is-sponsor)
  (cond [(symbol=? card-type 'standard)
         (cond [is-sponsor (calc-airmile-per-dollar-spent dollar 15)]
               [else (calc-airmile-per-dollar-spent dollar 20)])]
        [else
         (cond [is-sponsor (calc-airmile-per-dollar-spent dollar 10)]
               [else (calc-airmile-per-dollar-spent dollar 15)])]))

;; Tests
(check-expect (calc-airmiles 10.72 'premium true) 1)
(check-expect (calc-airmiles 9.72 'premium true) 0)
(check-expect (calc-airmiles 30 'standard false) 1)
(check-expect (calc-airmiles 30 'standard true) 2)
(check-expect (calc-airmiles 30 'premium false) 2)
(check-expect (calc-airmiles 30 'premium true) 3)