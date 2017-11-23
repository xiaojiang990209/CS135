;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 01, Problem 2
;; **********************************************
;;

;; Useful converters
(define mile->metre 1609.344)
(define hour->second 3600)
(define second->millifortnight 1209.6)
(define smoot->metre 1.7018)
(define gallon->litre 3.785411784)
(define 100km->metres 100000)

;; (a)
;; (mps->mph mps) converts a speed in the units of m/s to
;;    the same speed in units of mph
;; mps->mph: Num -> Num
;; Examples:
(check-expect (m/s->mph 10) 36000000/1609344)
(check-expect (m/s->mph 25) 90000000/1609344)
(check-expect (m/s->mph 80) 288000000/1609344)

(define (m/s->mph m/s)
  (/ (* m/s hour->second) mile->metre))

;; Tests:
(check-expect (m/s->mph 27.77) 99972000/1609344)
(check-expect (m/s->mph -10) -36000000/1609344)
(check-expect (m/s->mph 0) 0)


;; (b)
;; (mph->S/mfn mph) converts a speed in units of mph
;;    to the same speed in Smoots per millifortnight
;; mps->S/mfn: Num -> Num
;; Examples:
(check-expect (mph->S/mfn 1) 19466625024/61264800)
(check-expect (mph->S/mfn 5) 9733312512/6126480)

(define (mph->S/mfn mph)
  (*(/ (/ (* mph mile->metre)
           smoot->metre)
        hour->second) second->millifortnight))

;; Tests
(check-expect (mph->S/mfn 0) 0)
(check-expect (mph->S/mfn -2) -38933250048/61264800)


;; (c)
;; (mpg->L/100km mpg) converts a fuel efficiency in mpg to
;;    the same efficiency in units of L/100km
;; mpg->L/100km: Num -> Num
;; requires: mpg > 0
;; Examples:
(check-expect (mpg->L/100km 1) 3785411784/16093440)
(check-expect (mpg->L/100km 0.5) 3785411784/8046720)

(define (mpg->L/100km mpg)
  (/ 1 (/ (* (/ mpg gallon->litre) mile->metre) 100km->metres)))

;; Tests:
(check-expect (mpg->L/100km 0.33) 37854117840/53108352)
(check-expect (mpg->L/100km 100) 37854117840/16093440000)
