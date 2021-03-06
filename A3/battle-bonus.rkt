;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 03, Bonus
;; *******************************************************
;;

(define-struct card (strength colour))
;; A Card is a (make-card Nat Sym)
;; requires: 1 <= strength <= 9
;;           colour is one of: 'red 'yellow 'green 'blue 'purple 'brown 


;; my-card-fn: Card -> Any
(define (my-card-fn c)
  ( ... (card-strength c) ...
    ... (card-colour c) ...))


(define-struct hand (c1 c2 c3))
;;A Hand is a (make-hand Card Card Card)


;; my-hand-fn: Hand -> Any
(define (my-hand-fn hand)
  ( ... (hand-c1 hand) ...
    ... (hand-c2 hand) ...
    ... (hand-c3 hand) ...))


;; **********************************************************************
;; (create-hand card1 strength2 colour2 strength3 colour3) produces
;;    a Hand with card1, a card with strength2 and colour2, and
;;    a card with strength3 and colour3
;; create-hand: Card Nat Sym Nat Sym -> Hand
;; requires: 1 <= strength <= 9
;;           colour is one of: 'red 'yellow 'green 'blue 'purple 'brown  
;; Example:
(check-expect (create-hand myCard1 6 'red 7 'purple)
              (make-hand myCard1 (make-card 6 'red)
                         (make-card 7 'purple)))
               
(define (create-hand card1 strength2 colour2 strength3 colour3)
  (make-hand card1
             (make-card strength2 colour2)
             (make-card strength3 colour3)))


;; Useful constants for testing and examples
(define oppHand (make-hand (make-card 7 'blue)
                           (make-card 8 'blue)
                           (make-card 9 'blue)))

(define oppHand1 (create-hand (make-card 5 'red) 8 'red 9 'red))
(define myCard1 (make-card 6 'blue))
(define oppHand2 (create-hand (make-card 7 'red) 8 'red 9 'red))
(define myCard2 (make-card 9 'blue))
(define oppHand3 (create-hand (make-card 3 'red) 3 'blue 3 'purple))
(define myCard3 (make-card 2 'blue))


;; **********************************************************************
;; (colour? hand) produces true if the cards in hand has the
;;    same colour, and false otherwise
;; colour?: Hand -> Bool
;; Example
(check-expect (colour? oppHand1) true)

(define (colour? hand)
  (and (symbol=? (card-colour (hand-c1 hand))
                 (card-colour (hand-c2 hand)))
       (symbol=? (card-colour (hand-c1 hand))
                 (card-colour (hand-c3 hand)))))


;; (run? hand) produces true if the cards in hand form a run,
;;    and false otherwise
;; run?: Hand -> Bool
;; Example:
(check-expect (run? oppHand2) true)

(define (run? hand)
  (= (* 3 (+ 1 (min (card-strength (hand-c1 hand))
                    (card-strength (hand-c2 hand))
                    (card-strength (hand-c3 hand)))))
     (sum hand)))


;; (three-of-a-kind? hand) produces true if the cards in hand
;;    form a three-of-a-kind, and false otherwise
;; three-of-a-kind?: Hand -> Bool
;; Example:
(check-expect (three-of-a-kind? oppHand3) true)

(define (three-of-a-kind? hand)
  (and (= (card-strength (hand-c1 hand))
          (card-strength (hand-c2 hand)))
       (= (card-strength (hand-c1 hand))
          (card-strength (hand-c3 hand)))))


;; (colour-run? hand) produces true if the cards in hand form
;;    a colour-run, and false otherwise.
;; colour-run?: Hand -> Bool
;; Example:
(check-expect (colour-run? oppHand2) true)

(define (colour-run? hand)
  (and (colour? hand) (run? hand)))


;; (sum hand) produces the numerical sum of the strength of
;;    the cards in hand.
;; sum: Hand -> Nat
;; Example:
(check-expect (sum oppHand1) 22)

(define (sum hand)
  (+ (card-strength (hand-c1 hand))
     (card-strength (hand-c2 hand))
     (card-strength (hand-c3 hand))))


;; **********************************************************************
;; (duplicate-card? card1 card2) produces true if card1 and card2
;;    is the same Card, and false otherwise
;; duplicate-card?: Card Card -> Bool
;; Example:
(check-expect (duplicate-card? myCard1 myCard1) true)

(define (duplicate-card? card1 card2)
  (and (= (card-strength card1) (card-strength card2))
       (symbol=? (card-colour card1) (card-colour card2))))


;; (duplicate? hand1 hand2) produces true if there is a duplicate
;;    card within hand1 and hand2, and false otherwise.
;; duplicate?: Hand Hand -> Bool
;; Example
(check-expect (duplicate? oppHand1 oppHand2) true)

(define (duplicate? hand1 hand2)
  (or (duplicate-card? (hand-c1 hand1) (hand-c1 hand2))
      (duplicate-card? (hand-c1 hand1) (hand-c2 hand2))
      (duplicate-card? (hand-c1 hand1) (hand-c3 hand2))
      (duplicate-card? (hand-c2 hand1) (hand-c1 hand2))
      (duplicate-card? (hand-c2 hand1) (hand-c2 hand2))
      (duplicate-card? (hand-c2 hand1) (hand-c3 hand2))
      (duplicate-card? (hand-c3 hand1) (hand-c1 hand2))
      (duplicate-card? (hand-c3 hand1) (hand-c2 hand2))
      (duplicate-card? (hand-c3 hand1) (hand-c3 hand2))))


;; **********************************************************************
;; (sub1-card card) produces a Card with one less strength than the
;;    strength of card and with the same colour as card.
;; sub1-card: Card -> Card
;; Example:
(check-expect (sub1-card myCard1)
              (make-card (- (card-strength myCard1) 1)
                         (card-colour myCard1)))

(define (sub1-card card)
  (make-card (- (card-strength card) 1) (card-colour card)))


;; (sub2-card card) produces a Card with two less strength than the
;;    strength of card and with the same colour as card.
;; sub1-card: Card -> Card
;; Example:
(check-expect (sub2-card myCard1)
              (make-card (- (card-strength myCard1) 2)
                         (card-colour myCard1)))

(define (sub2-card card)
  (make-card (- (card-strength card) 2) (card-colour card)))


;; **********************************************************************
;; (create-colour-run-hand card1) produces a colour-run hand based
;;    on card1 the user has played.
;; create-colour-run-hand: Card -> Hand
;; Example:
(check-expect (create-colour-run-hand myCard1)
              (create-hand myCard1 7 'blue 8 'blue))

(define (create-colour-run-hand card1)
  (cond [(= (card-strength card1) 9)
         (create-hand card1
                      8 (card-colour card1)
                      7 (card-colour card1))]
        [(= (card-strength card1) 8)
         (create-hand card1
                      7 (card-colour card1)
                      9 (card-colour card1))]
        [else
         (create-hand card1
                      (+ 1 (card-strength card1)) (card-colour card1)
                      (+ 2 (card-strength card1)) (card-colour card1))]))


;; (create-three-of-a-kind-hand card colour1 colour2) produces a
;;    three-of-a-kind hand with card, colour1 and colour2
;; create-three-of-a-kind-hand: Card Sym Sym -> Hand
;; Example:
(check-expect (create-three-of-a-kind-hand
               (make-card 3 'red) 'blue 'purple) oppHand3)

(define (create-three-of-a-kind-hand card colour1 colour2)
  (create-hand card (card-strength card) colour1 (card-strength card) colour2))


;; **********************************************************************
;; (valid? card) produces true if the strength of card is between
;;    1 and 9, inclusive, and false otherwise
;; valid?: Card -> Bool
;; Example:
(check-expect (valid? (make-card 11 'blue)) false)

(define (valid? card)
  (and (<= (card-strength card) 9) (>= (card-strength card) 1)))


;;********************************************************
;; (can-win-colour-run? card opponent-hand) produces true if it is
;;    possible for the player to beat opponent-hand based on card,
;;    and false otherwise, provided opponent-hand is colour-run
;; can-win-colour-run?: Card Hand -> Bool
;; Example:
(check-expect (can-win-colour-run? myCard2 oppHand2) false)

(define (can-win-colour-run? card opponent-hand)
  (< (sum opponent-hand) (* (min (+ 1 (card-strength card)) 8) 3)))


;; (can-win-three-of-a-kind? card opponent-hand) produces true if
;;    it is possible for the player to beat opponent-hand based
;;    on card, and false otherwise, provided opponent-hand is also
;;    three-of-a-kind.
;; can-win-three-of-a-kind?: Card Hand -> Bool
;; Example:
(check-expect (can-win-three-of-a-kind? myCard3 oppHand3) false)

(define (can-win-three-of-a-kind? card opponent-hand)
  (< (card-strength (hand-c1 opponent-hand)) (card-strength card)))


;;*********************************************************
;; (find-winner card opponent-hand) produces the hand that will defeat
;;    opponent-hand, provided the user has played card, and false if
;;    no such hand exists
;; find-winner: Card Hand -> (anyof Hand false)
;; Example:
(check-expect (find-winner myCard1 oppHand1)
              (create-hand myCard1 7 'blue 8 'blue))

(define (find-winner card opponent-hand)
  (cond [(and (colour-run? opponent-hand)
              (not (can-win-colour-run? card opponent-hand))) false]
        [(not (duplicate? opponent-hand (create-colour-run-hand card)))
         (create-colour-run-hand card)]
        [(and (valid? (sub1-card card))
              (not (duplicate? opponent-hand
                               (create-colour-run-hand (sub1-card card)))))
         (create-colour-run-hand (sub1-card card))]
        [(and (valid? (sub2-card card))
              (not (duplicate? opponent-hand
                               (create-colour-run-hand (sub2-card card)))))
         (create-colour-run-hand (sub2-card card))]
        [(and (three-of-a-kind? opponent-hand)
              (not (can-win-three-of-a-kind? card opponent-hand))) false]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'red 'yellow)))
         (create-three-of-a-kind-hand card 'red 'yellow)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'red 'green)))
         (create-three-of-a-kind-hand card 'red 'green)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'red 'blue)))
         (create-three-of-a-kind-hand card 'red 'blue)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'red 'purple)))
         (create-three-of-a-kind-hand card 'red 'purple)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'red 'brown)))
         (create-three-of-a-kind-hand card 'red 'brown)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'yellow 'green)))
         (create-three-of-a-kind-hand card 'yellow 'green)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'yellow 'blue)))
         (create-three-of-a-kind-hand card 'yellow 'blue)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'yellow 'purple)))
         (create-three-of-a-kind-hand card 'yellow 'purple)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'yellow 'brown)))
         (create-three-of-a-kind-hand card 'yellow 'brown)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'green 'blue)))
         (create-three-of-a-kind-hand card 'green 'blue)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'green 'purple)))
         (create-three-of-a-kind-hand card 'green 'purple)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'green 'brown)))
         (create-three-of-a-kind-hand card 'green 'brown)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'blue 'purple)))
         (create-three-of-a-kind-hand card 'blue 'purple)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'blue 'brown)))
         (create-three-of-a-kind-hand card 'blue 'brown)]
        [(not (duplicate? opponent-hand
                          (create-three-of-a-kind-hand card 'purple 'brown)))
         (create-three-of-a-kind-hand card 'purple 'brown)]
        [else false]))

;; Tests:
(check-expect (find-winner myCard2 oppHand2) false)
(check-expect (find-winner myCard3 oppHand3) false)
