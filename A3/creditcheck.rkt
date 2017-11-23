;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname creditcheck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 3
;; *******************************************************
;;

(define-struct date (year month day))
;; A Date is a (make-date Nat Nat Nat)
;; requires: year/month/day corresponds to a valid date
;;           (in the Gregorian calendar)


;; my-date-fn: Date -> Any
(define (my-date-fn date)
  ( ... (date-year date) ...
    ... (date-month date) ...
    ... (date-day date) ...))


(define-struct transaction (tdate amount category))
;; A Transaction is a (make-transaction Date Num Sym)


;; my-transaction-fn: Transaction -> Any
(define (my-transaction-fn transaction)
  ( ... (transaction-tdate transaction) ...
    ... (transaction-amount transaction) ...
    ... (transaction-category transaction) ...))


(define-struct account (name expires limit threshold exception))
;; An Account is a (make-account Str Date Num Num Sym)
;; requires: 0 < threshold < limit


;; my-account-fn: Account -> Any
(define (my-account-fn account)
  ( ... (account-name account) ...
    ... (account-expires account) ...
    ... (account-limit account) ...
    ... (account-threshold account) ...
    ... (account-exception account) ...))


;; Useful constants for examples and testing
(define 2018-08-25 (make-date 2018 08 25))
(define 2017-08-20 (make-date 2017 08 20))
(define 2017-09-25 (make-date 2017 09 25))
(define 2017-08-30 (make-date 2017 08 30))
(define 2017-08-19 (make-date 2017 08 19))
(define my-account
  (make-account "Name" (make-date 2017 09 20) 1000 500 'drink))
(define valid-transaction
  (make-transaction (make-date 2016 09 20) 100 'food))
(define expired-transaction
  (make-transaction (make-date 2018 09 20) 100 'food))
(define on-expiry-date-transaction
  (make-transaction (make-date 2017 09 20) 100 'food))
(define over-limit-transaction
  (make-transaction (make-date 2016 09 20) 5000 'food))
(define on-limit-transaction
  (make-transaction (make-date 2016 09 20) 1000 'food))
(define over-threshold-not-excepted-transaction
  (make-transaction (make-date 2016 09 20) 600 'food))
(define on-threshold-not-excepted-transaction
  (make-transaction (make-date 2016 09 20) 500 'food))
(define over-threshold-excepted-transaction
  (make-transaction (make-date 2016 09 20) 999 'drink))
(define on-threshold-excepted-transaction
  (make-transaction (make-date 2016 09 20) 500 'drink))
  


;; *****************************************************************
;; (a)
;; (date<=? date1 date2) consumes date1, date2 and produces true if
;;    date1 occurs before date2 or date1 and date2 are the
;;    same Date, and produces false otherwise
;; date<=?: Date Date -> Bool
;; Example:
(check-expect (date<=? 2018-08-25 2017-08-20) false)

(define (date<=? date1 date2)
  (cond [(> (date-year date1) (date-year date2)) false]
        [(> (date-month date1) (date-month date2)) false]
        [(> (date-day date1) (date-day date2)) false]
        [else true]))

;; Test
(check-expect (date<=? 2017-09-25 2017-08-20) false)
(check-expect (date<=? 2017-08-30 2017-08-20) false)
(check-expect (date<=? 2017-08-20 2017-08-20) true)
(check-expect (date<=? 2017-08-19 2017-08-20) true)


;; *****************************************************************
;; (b)
;; (approve? transaction account) produces true if the amount of
;;    transaction does not exceed the limit of account and the
;;    transaction date is not after the date the card expires,
;;    and false otherwise
;; approve?: Transaction Account -> Bool
;; Example:
(check-expect (approve? valid-transaction my-account) true)

(define (approve? transaction account)
  (and (date<=? (transaction-tdate transaction)
                (account-expires account))
       (<= (transaction-amount transaction)
           (account-limit account))))

;; Tests:
(check-expect (approve? expired-transaction my-account) false)
(check-expect (approve? over-limit-transaction my-account) false)
(check-expect (approve? on-limit-transaction my-account) true)
(check-expect (approve? on-expiry-date-transaction my-account) true)


;; *****************************************************************
;; (c)
;; (alert? transaction account) produces true if the transaction is
;;   approved, but the transaction amount exceeds the threshold and
;;   the transaction category is not the exception category in the
;;   account, and false otherwise
;; alert?: Transaction Account -> Bool
;; Example:
(check-expect (alert? valid-transaction my-account) false)
(check-expect (alert? expired-transaction my-account) false)


(define (alert? transaction account)
  (cond [(not (approve? transaction account)) false]
        [(symbol=? (transaction-category transaction)
                   (account-exception account))
         false]
        [(<= (transaction-amount transaction)
             (account-threshold account))
         false]
        [else true]))

;; Tests:
(check-expect (alert? on-expiry-date-transaction my-account) false)
(check-expect (alert? over-threshold-excepted-transaction my-account)
              false)
(check-expect (alert? over-threshold-not-excepted-transaction
                      my-account) true)
(check-expect (alert? on-threshold-not-excepted-transaction
                      my-account) false)
(check-expect (alert? on-threshold-excepted-transaction
                      my-account) false)