;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 3
;; **********************************************
;;


;; Useful constants
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define assignment-weight 0.2)
(define participation-weight 0.05)
(define total-exam-weight (+ first-midterm-weight second-midterm-weight
                             final-exam-weight))
(define passing-grade 50)
(define minimum-grade 46)


;; (calc-weighted-exam-grade first-midterm-grade second-midterm-grade
;;   final-exam-grade) computes the weighted exam average of cs135
;;   based on first-midterm-grade, second-midterm-grade and
;;   final-exam-grade
;; calc-weighted-exam-grade: Nat Nat Nat -> Num
;; Example:
(check-expect (calc-weighted-exam-grade 70 75 77) 5665/75)

(define (calc-weighted-exam-grade first-midterm-grade
                                  second-midterm-grade
                                  final-exam-grade)
  (/ (+ (* first-midterm-grade first-midterm-weight)
        (* second-midterm-grade second-midterm-weight)
        (* final-exam-grade final-exam-weight))
     total-exam-weight))


;; (calc-normal-final-grade weighted-exam-grade assignment-grade
;;   participation-grade) calculates the normal final grade using
;;   weighted-exam-grade, assignment-grade and participation-grade
;; calc-normal-final-grade: Num Nat Nat -> Num
;; requires: weighted-exam-grade >= 0
;; Example:
(check-expect (calc-normal-final-grade 77 85 95) 79.5)

(define (calc-normal-final-grade weighted-exam-grade assignment-grade
                                 participation-grade)
  (+ (* weighted-exam-grade total-exam-weight)
     (* assignment-grade assignment-weight)
     (* participation-grade participation-weight)))


;; (final-cs135-grade first-midterm-grade second-midterm-grade
;;   final-exam-grade assignment-grade participation-grade)
;;   produces the final grade in the course based on the
;;   first-midterm-grade, second-midterm-grade, assigment-grade,
;;   and participation-grade
;; final-cs135-grade: Nat Nat Nat Nat Nat -> Num
;; Examples:
(check-expect (final-cs135-grade 70 75 77 85 95) 78.4)
(check-expect (final-cs135-grade 50 50 50 45 100) 46)

(define (final-cs135-grade first-midterm-grade second-midterm-grade
                           final-exam-grade assignment-grade
                           participation-grade)
  (cond [(or (< (calc-weighted-exam-grade first-midterm-grade
                                          second-midterm-grade
                                          final-exam-grade)
                passing-grade)
             (< assignment-grade passing-grade))
         [cond [(> (calc-normal-final-grade
                    (calc-weighted-exam-grade first-midterm-grade
                                              second-midterm-grade
                                              final-exam-grade)
                    assignment-grade participation-grade)
                   minimum-grade)
                minimum-grade]
               [else (calc-normal-final-grade
                    (calc-weighted-exam-grade first-midterm-grade
                                              second-midterm-grade
                                              final-exam-grade)
                    assignment-grade participation-grade)]]]
         [else (calc-normal-final-grade
                    (calc-weighted-exam-grade first-midterm-grade
                                              second-midterm-grade
                                              final-exam-grade)
                    assignment-grade participation-grade)]))

;; Tests:
(check-expect (final-cs135-grade 80 70 75 80 100) 76.75)
(check-expect (final-cs135-grade 50 50 40 30 100) 44)
(check-expect (final-cs135-grade 50 50 50 50 100) 52.5)
(check-expect (final-cs135-grade 50 50 50 49 100) 46)
(check-expect (final-cs135-grade 49 49 49 50 100) 46)
(check-expect (final-cs135-grade 46 46 46 46 46) 46)
(check-expect (final-cs135-grade 45 45 45 45 45) 45)