;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 02, Bonus Question
;; **********************************************
;;


;; (can-donate-to/bool? donor-blood-type recipient-blood-type)
;;   produces true if the donor-blood-type is acceptable for the
;;   recipient-blood-type, and false otherwise.
;; can-donate-to/bool?: Sym Sym -> Bool
;; Examples
(check-expect (can-donate-to/bonus? 'A- 'AB-) true)
(check-expect (can-donate-to/bonus? 'A+ 'B-) false)
(check-expect (can-donate-to/bonus? 'A+ 'AB+) true)

(define (can-donate-to/bonus? donor-blood-type recipient-blood-type)
  (not (and (not (symbol=? donor-blood-type recipient-blood-type))
            (not (symbol=? donor-blood-type 'O-))
            (not (symbol=? recipient-blood-type 'AB+))
            (not (and (symbol=? donor-blood-type 'O+)
                      (symbol=? recipient-blood-type 'A+)))
            (not (and (symbol=? donor-blood-type 'O+)
                      (symbol=? recipient-blood-type 'B+)))
            (not (and (symbol=? donor-blood-type 'A-)
                      (symbol=? recipient-blood-type 'A+)))
            (not (and (symbol=? donor-blood-type 'A-)
                      (symbol=? recipient-blood-type 'AB-)))
            (not (and (symbol=? donor-blood-type 'B-)
                      (symbol=? recipient-blood-type 'B+)))
            (not (and (symbol=? donor-blood-type 'B-)
                      (symbol=? recipient-blood-type 'AB-))))))

;; Tests
(check-expect (can-donate-to/bonus? 'O+ 'B-) false)
(check-expect (can-donate-to/bonus? 'B- 'B+) true)
(check-expect (can-donate-to/bonus? 'B- 'AB-) true)
(check-expect (can-donate-to/bonus? 'O- 'B+) true)
(check-expect (can-donate-to/bonus? 'O+ 'A+) true)
(check-expect (can-donate-to/bonus? 'O+ 'B+) true)
(check-expect (can-donate-to/bonus? 'A- 'A+) true)
(check-expect (can-donate-to/bonus? 'A- 'B+) false)
(check-expect (can-donate-to/bonus? 'B- 'O+) false)