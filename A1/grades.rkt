;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 01, Problem 3
;; **********************************************
;;

;; Useful constants
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define assignment-weight 0.2)
(define participation-mark 5)
(define minimum-passing-grade 60)

;; (a)
;; (final-cs135-grade first_midterm second_midterm final_exam assignment)
;;   calculates the final grade of CS135 based on the midterm grades, final
;;   grade and the assignments grade
;; final-cs135-grade : Num Num Num Num -> Num
;; requires: 0 <= first_midterm <= 100
;;           0 <= second_midterm <= 100
;;           0 <= final_exam <= 100
;;           0 <= assignment <= 100
;; Examples:
(check-expect (final-cs135-grade 70 75 80 75) 78)
(check-expect (final-cs135-grade 80 78 72 85) 78)
(check-expect (final-cs135-grade 60 64 68 65) 67.4)

(define (final-cs135-grade first_midterm second_midterm
                           final_exam assignment)
  (+ participation-mark
     (+ (+ (+ (* first_midterm first-midterm-weight)
              (* second_midterm second-midterm-weight))
           (* final_exam final-exam-weight))
        (* assignment assignment-weight))))

;; Tests:
(check-expect (final-cs135-grade 0 0 0 0) participation-mark)
(check-expect (final-cs135-grade 100 100 100 100) 100)


;; (b)
;; (cs135-final-exam-grade-needed first_midterm second_midterm assignment)
;;   calculates the minimum grade needed on the final exam to obtain
;;   60% in CS135
;; cs135-final-exam-grade-needed : Num Num Num -> Num
;; requires: 0 <= first_midterm <= 100
;;           0 <= second_midterm <= 100
;;           0 <= assignment <= 100
;; Example:
(check-expect (cs135-final-exam-grade-needed 60 63 80) 136/3)
(check-expect (cs135-final-exam-grade-needed 60 58 61) 56)
(check-expect (cs135-final-exam-grade-needed 80 70 74) 364/9)

(define (cs135-final-exam-grade-needed first_midterm
                                       second_midterm assignment)
  (/ (- (- (- (- minimum-passing-grade participation-mark)
              (* first-midterm-weight first_midterm))
           (* second-midterm-weight second_midterm))
        (* assignment-weight assignment))
     final-exam-weight))

;; Tests:
(check-expect (cs135-final-exam-grade-needed 0 0 0) 1100/9)
(check-expect (cs135-final-exam-grade-needed 100 100 100) 100/9)