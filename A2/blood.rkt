;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 4
;; **********************************************
;;


;; (can-donate-to/cond? donor-blood-type recipient-blood-type)
;;   produces true if the donor-blood-type is acceptable for the
;;   recipient-blood-type, and false otherwise.
;; can-donate-to/cond?: Sym Sym -> Bool
;; Examples
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'A+ 'B-) false)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)

(define (can-donate-to/cond? donor-blood-type recipient-blood-type)
  (cond [(symbol=? donor-blood-type recipient-blood-type) true]
        [(symbol=? donor-blood-type 'O-) true]
        [(symbol=? recipient-blood-type 'AB+) true]
        [(symbol=? donor-blood-type 'O+)
         (cond [(symbol=? recipient-blood-type 'A+) true]
               [(symbol=? recipient-blood-type 'B+) true]
               [else false])]
        [(symbol=? donor-blood-type 'A-)
         (cond [(symbol=? recipient-blood-type 'A+) true]
               [(symbol=? recipient-blood-type 'AB-) true]
               [else false])]
        [(symbol=? donor-blood-type 'B-)
         (cond [(symbol=? recipient-blood-type 'B+) true]
               [(symbol=? recipient-blood-type 'AB-) true]
               [else false])]
        [else false]))

;; Tests
(check-expect (can-donate-to/cond? 'O+ 'B-) false)
(check-expect (can-donate-to/cond? 'AB- 'AB-) true)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
(check-expect (can-donate-to/cond? 'O- 'B+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'B+) false)
(check-expect (can-donate-to/cond? 'B- 'O+) false)

              
;; (can-donate-to/bool? donor-blood-type recipient-blood-type)
;;   produces true if the donor-blood-type is acceptable for the
;;   recipient-blood-type, and false otherwise.
;; can-donate-to/bool?: Sym Sym -> Bool
;; Examples
(check-expect (can-donate-to/bool? 'A- 'AB-) true)
(check-expect (can-donate-to/bool? 'A+ 'B-) false)
(check-expect (can-donate-to/bool? 'A+ 'AB+) true)

(define (can-donate-to/bool? donor-blood-type recipient-blood-type)
  (or (symbol=? donor-blood-type recipient-blood-type)
      (symbol=? donor-blood-type 'O-)
      (symbol=? recipient-blood-type 'AB+)
      (and (symbol=? donor-blood-type 'O+)
           (symbol=? recipient-blood-type 'A+))
      (and (symbol=? donor-blood-type 'O+)
           (symbol=? recipient-blood-type 'B+))
      (and (symbol=? donor-blood-type 'A-)
           (symbol=? recipient-blood-type 'A+))
      (and (symbol=? donor-blood-type 'A-)
           (symbol=? recipient-blood-type 'AB-))
      (and (symbol=? donor-blood-type 'B-)
           (symbol=? recipient-blood-type 'B+))
      (and (symbol=? donor-blood-type 'B-)
           (symbol=? recipient-blood-type 'AB-))))

;; Tests
(check-expect (can-donate-to/bool? 'O+ 'B-) false)
(check-expect (can-donate-to/bool? 'AB- 'AB-) true)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'B- 'AB-) true)
(check-expect (can-donate-to/bool? 'O- 'B+) true)
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'B+) true)
(check-expect (can-donate-to/bool? 'A- 'A+) true)
(check-expect (can-donate-to/bool? 'A- 'B+) false)
(check-expect (can-donate-to/bool? 'B- 'O+) false)
