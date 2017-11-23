;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 4
;; *******************************************************
;;

(define-struct card (strength colour))
;; A Card is a (make-card Nat Sym)
;; requires: 1 <= strength <= 9
;;           color is one of: 'red 'yellow 'green 'blue 'purple 'brown


;; my-card-fn: Card -> Any
(define (my-card-fn c)
  ( ... (card-strength c) ...
    ... (card-colour c) ...))


(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;; my-hand-fn: Hand -> Any
(define (my-hand-fn hand)
  ( ... (hand-c1 hand) ...
    ... (hand-c2 hand) ...
    ... (hand-c3 hand) ...))


;; Useful constants for examples and testing
(define colour-run-hand
  (make-hand (make-card 5 'red)
             (make-card 6 'red)
             (make-card 7 'red)))
(define three-of-a-kind-hand
  (make-hand (make-card 5 'red)
             (make-card 5 'yellow)
             (make-card 5 'blue)))
(define colour-hand
  (make-hand (make-card 1 'red)
             (make-card 5 'red)
             (make-card 7 'red)))
(define run-hand
  (make-hand (make-card 1 'red)
             (make-card 2 'blue)
             (make-card 3 'yellow)))
(define normal-hand
  (make-hand (make-card 1 'red)
             (make-card 3 'blue)
             (make-card 5 'yellow)))
(define normal-hand-sum 9)
(define larger-normal-hand
  (make-hand (make-card 3 'red)
             (make-card 5 'blue)
             (make-card 7 'yellow)))
(define larger-normal-hand-sum 15)
                        

;; (colour? hand) produces true if the Cards in hand all
;;   have the same colour, and false otherwise
;; colour?: Hand -> Bool
;; Example:
(check-expect (colour? colour-hand) true)

(define (colour? hand)
  (and (symbol=? (card-colour (hand-c1 hand))
                 (card-colour (hand-c2 hand)))
       (symbol=? (card-colour (hand-c1 hand))
                 (card-colour (hand-c3 hand)))))


;; (run? hand) produces true if the Cards in hand form a
;;   run, and false otherwise
;; run?: Hand -> Bool
;; Example:
(check-expect (run? run-hand) true)
(check-expect (run? colour-run-hand) true)

(define (run? hand)
  (= (* 3 (+ 1 (min (card-strength (hand-c1 hand))
                    (card-strength (hand-c2 hand))
                    (card-strength (hand-c3 hand)))))
     (sum hand)))


;; (three-of-a-kind? hand) produces true if the Cards in hand
;;   have the same strength, and false otherwise
;; three-of-a-kind?: Hand -> Bool
;; Example:
(check-expect (three-of-a-kind? three-of-a-kind-hand) true)

(define (three-of-a-kind? hand)
  (and (= (card-strength (hand-c1 hand))
          (card-strength (hand-c2 hand)))
       (= (card-strength (hand-c1 hand))
          (card-strength (hand-c3 hand)))))


;; (colour-run? hand) produces true if the Cards in hand
;;   all have the same colour and form a run, and
;;   false otherwise
;; colour-run?: Hand -> Bool
;; Example:
(check-expect (colour-run? colour-run-hand) true)
(check-expect (colour-run? run-hand) false)
(check-expect (colour-run? colour-hand) false)

(define (colour-run? hand)
  (and (colour? hand) (run? hand)))


;; (sum hand) produces the sum of the Cards in hand
;; sum: Hand -> Nat
;; Example:
(check-expect (sum normal-hand) normal-hand-sum)
(check-expect (sum larger-normal-hand) larger-normal-hand-sum)

(define (sum hand)
  (+ (card-strength (hand-c1 hand))
     (card-strength (hand-c2 hand))
     (card-strength (hand-c3 hand))))


;; (battle hand1 hand2) produces 'player1 if either hand1
;;   defeats hand2 or there is a tie. Otherwise, it produces
;;   'player2
;; battle: Hand Hand -> Sym
;; Example:
(check-expect (battle colour-run-hand run-hand) 'player1)

(define (battle hand1 hand2)
  (cond [(and (colour-run? hand1)
              (not (colour-run? hand2)))
         'player1]
        [(and (not (colour-run? hand1))
              (colour-run? hand2))
         'player2]
        [(and (three-of-a-kind? hand1)
              (not (three-of-a-kind? hand2)))
         'player1]
        [(and (not (three-of-a-kind? hand1))
              (three-of-a-kind? hand2))
         'player2]
        [(and (colour? hand1) (not (colour? hand2)))
         'player1]
        [(and (not (colour? hand1)) (colour? hand2))
         'player2]
        [(and (run? hand1) (not (run? hand2)))
         'player1]
        [(and (not (run? hand1)) (run? hand2))
         'player2]
        [(> (sum hand1) (sum hand2))
         'player1]
        [(< (sum hand1) (sum hand2))
         'player2]
        [else 'player1]))

;; Tests
(check-expect (battle three-of-a-kind-hand colour-run-hand) 'player2)
(check-expect (battle three-of-a-kind-hand colour-hand) 'player1)
(check-expect (battle colour-hand three-of-a-kind-hand) 'player2)
(check-expect (battle colour-hand run-hand) 'player1)
(check-expect (battle run-hand colour-hand) 'player2)
(check-expect (battle run-hand normal-hand) 'player1)
(check-expect (battle normal-hand run-hand) 'player2)
(check-expect (battle larger-normal-hand normal-hand) 'player1)
(check-expect (battle normal-hand larger-normal-hand) 'player2)
(check-expect (battle normal-hand normal-hand) 'player1)
