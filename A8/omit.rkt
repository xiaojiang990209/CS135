;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname omit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 8, Bonus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Idea:
;; Same as omit2
(define (omit1 str)
  (omit2 str))


;; Idea:
;; Suppose we have "abc", foldr will separate it into #\a #\b #\c, and we get to the base case
;; From the base case up, suppose we have a base case of '(""), then we can insert our last
;; visited character, in this case #\c, into the result list, while concatenating the previously
;; generated list at the end of the result list.
;;
;; i.e., when we handle #\c, we have (append '("c") '("")) => '("c" "")
;; When we have #\b, we will have (append '("b") '("c" "")) => '("bc" "b" "c" "")
;; When we finally get to #\a, we will have:
;; (append '("a") '("bc" "b" "c" "")) => '("abc" "ab" "ac" "a" "bc" "b" "c" "")
;; which is our desired result.

(define (omit2 str)
  (foldr (lambda (f r) (append (build-list (length r)
  (lambda (i) (list->string (append (list f) (string->list (list-ref r i))))))
  r)) '("") (string->list str)))

;;(define (omit3 str)
;;  ((lambda (loc)
;;     (((lambda (x) (x x))
;;       (lambda (omit-char)
;;         (lambda (lst acc)
;;           (cond [(empty? lst) (list (list->string (append acc lst)))]
;;                 [else
;;                  (append
;;                   ((omit-char omit-char) (rest lst) (append acc (list (first lst))))
;;                   ((omit-char omit-char) (rest lst) acc))])))) loc empty))
;;   (string->list str)))

                     

