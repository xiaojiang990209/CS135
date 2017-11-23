;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 01, Bonus Question
;; **********************************************
;;

;; Useful constants
(define minimum-final-mark 46)
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define exam-weight 0.75)
(define assignment-weight 0.2)
(define participation-mark 5)
(define minimum-passing-mark 50)

;; (calculate-weighted-exam-component first-midterm second-midterm final-exam)
;;    Produces the final mark of the exam component of CS135
;; calculate-weighted-exam-component: Num Num Num -> Num
;; requires: 0 <= first-midterm <= 100
;;           0 <= second-midterm <= 100
;;           0 <= final-exam <= 100
;; Example:
(check-expect (calculate-weighted-exam-component 70 65 60) 188/3)

(define (calculate-weighted-exam-component first-midterm
                                           second-midterm final-exam)
  (/ (+ (+ (* first-midterm-weight first-midterm)
           (* second-midterm-weight second-midterm))
        (* final-exam-weight final-exam))
     exam-weight))


;; (calculate-normal-final-grade weighted-exam assignment) Produces the
;;    normal final grade of the course.
;; calculate-normal-final-grade: Num Num -> Num
;; requires: 0 <= weighted-exam <= 100
;;           0 <= assignment <= 100
;; Example:
(check-expect (calculate-normal-final-grade 50 60) 54.5)
(check-expect (calculate-normal-final-grade 0 0) 5)

(define (calculate-normal-final-grade weighted-exam assignment)
  (+ (+ (* exam-weight weighted-exam)
        (* assignment-weight assignment))
     participation-mark))


;; (passing-indicator weighted-exam assignment) Produces 1 if either
;;    of the weighted-exam or the assignment falls below 50, otherwise
;;    produces 0
;; passing-indicator: Num Num -> Num
;; requires: 0 <= weighted-exam <= 100
;;           0 <= assignment <= 100
;; Example:
(check-expect (passing-indicator 60 80) 0)
(check-expect (passing-indicator 49 50) 1)
(check-expect (passing-indicator 49 49) 1)

(define (passing-indicator weighted-exam assignment)
  (abs (sgn (+ (- (min weighted-exam minimum-passing-mark) minimum-passing-mark)
               (- (min assignment minimum-passing-mark) minimum-passing-mark)))))


;; (final-cs135-grade first-midterm second-midterm final-exam assignment)
;;    calculates the final grade of CS135 based on its individual
;;    components with respect to the rule of simultaneously passing the exam
;;    and the assignment component of the course
;; final-cs135-grade: Nat Nat Nat Nat -> Num
;; require: 0 <= first-midterm <= 100
;;          0 <= second-midterm <= 100
;;          0 <= final-exam <= 100
;;          0 <= assignment <= 100
;; Examples:
(check-expect (final-cs135-grade 70 65 60 50) 62)
(check-expect (final-cs135-grade 50 50 50 45) 46)

(define (final-cs135-grade first-midterm second-midterm final-exam assignment)
  (+ (* (- 1 (passing-indicator (calculate-weighted-exam-component first-midterm second-midterm final-exam)
                                assignment))
        (calculate-normal-final-grade
         (calculate-weighted-exam-component first-midterm second-midterm final-exam)
         assignment))
     (* (passing-indicator (calculate-weighted-exam-component first-midterm second-midterm final-exam)
                           assignment)
        (min (calculate-normal-final-grade
              (calculate-weighted-exam-component first-midterm second-midterm final-exam)
              assignment)
             minimum-final-mark))))

;; Test
(check-expect (final-cs135-grade 80 70 75 80) 76.75)
(check-expect (final-cs135-grade 60 50 30 50) 44.5)
(check-expect (final-cs135-grade 50 50 50 49) 46)