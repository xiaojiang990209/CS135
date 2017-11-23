;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pronunciation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang  (20714726)
;; CS135 Fall 2017
;; Assignment 05, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the code in this file.
;; Do not remove this line.
(require "pronunciationlib.rkt")

;; The data definitions as given in the question.

;; A Vowel is a (list Sym (anyof 0 1 2))

;; A Phoneme is an (anyof Sym Vowel)

;; A Pronunciation is a (listof Phoneme)
;; requires: the list contains exactly one vowel with a stress of 1

;; A Dictionary is a (listof (list Str Pronunciation))
;; requires: The strings in each sub-list appear in alphabetical
;;           order in the Dictionary.


;; -------------------------- (A) --------------------------
;; (count-syllables pronunciation) produces a natural number
;;   saying how many syllables are in the given pronunciation
;; count-syllables: Pronunciation -> Nat
;; Example
(check-expect (count-syllables '(B L (ER 1) T)) 1)

(define (count-syllables pronunciation)
  (cond [(empty? pronunciation) 0]
        [(cons? (first pronunciation))
         (+ 1 (count-syllables (rest pronunciation)))]
        [else
         (count-syllables (rest pronunciation))]))


;; (num-syllables word dictionary) produces a natural number saying
;;   how many syllables are in word, according to dictionary
;; num-syllables: Str Dictonary -> Nat
;; Example
(check-expect (num-syllables "describe" toy-dictionary) 2)

(define (num-syllables word dictionary)
  (cond [(empty? dictionary) 0]
        [(string=? (first (first dictionary)) word)
         (count-syllables (second (first dictionary)))]
        [else (num-syllables word (rest dictionary))]))

;; Tests
(check-expect (num-syllables "actress" empty) 0)
(check-expect (num-syllables "actress" toy-dictionary) 2)
(check-expect (num-syllables "blurt" toy-dictionary) 1)
(check-expect (num-syllables "significantly" toy-dictionary) 5)


;; -------------------------- (B) --------------------------
;; (has-no-vowel? pronunciation) produces true if pronunciation
;;   does not contain any Vowel, and false otherwise
;; has-no-vowel?: Pronunciation -> Bool
;; Example
(check-expect (has-no-vowel? '(AH G L IY)) true)

(define (has-no-vowel? pronunciation)
  (cond [(empty? pronunciation) true]
        [else
         (and (not (cons? (first pronunciation)))
              (has-no-vowel? (rest pronunciation)))]))


;; (equal-stress-pattern? pattern pronunciation) produces true
;;   if the stress pattern of pronunciation is equal to pattern
;; match-stress-pattern: (listof Nat) Pronunciation -> Bool
;; Example
(check-expect (equal-stress-pattern? '(0 1) '(P (AH 0) T (IY 1) T))
              true)
(check-expect (equal-stress-pattern? '(1 0) '((AH 1) G L (IY 0)))
              true)
(check-expect (equal-stress-pattern? '(0 1 0 0 0)
               '(S (IH 0) G N (IH 1) F (IH 0) K (AH 0) N T L (IY 0)))
              true)

(define (equal-stress-pattern? pattern pronunciation)
  (cond [(and (empty? pattern) (empty? pronunciation)) true]
        [(and (empty? pattern) (cons? pronunciation))
         (has-no-vowel? pronunciation)]
        [(and (cons? pattern) (empty? pronunciation)) false]
        [(cons? (first pronunciation))
         (and (= (first pattern) (second (first pronunciation)))
              (equal-stress-pattern? (rest pattern)
                                    (rest pronunciation)))]
        [else (equal-stress-pattern? pattern (rest pronunciation))]))


;; (find-stress-pattern pattern dictionary) produces a list of
;;   words in dictionary whose stress pattern is equal to pattern.
;; find-stress-pattern: (listof Nat) Dictionary -> (listof Str)
;; Example
(check-expect (find-stress-pattern '(0 1) toy-dictionary)
              (list "adopt" "concrete" "deprive" "describe" "petite"))

(define (find-stress-pattern pattern dictionary)
  (cond [(or (empty? pattern) (empty? dictionary)) empty]
        [(equal-stress-pattern? pattern (second (first dictionary)))
         (cons (first (first dictionary))
               (find-stress-pattern pattern (rest dictionary)))]
        [else (find-stress-pattern pattern (rest dictionary))]))

;; Tests:
(check-expect (find-stress-pattern empty empty) empty)
(check-expect (find-stress-pattern '(0 1) empty) empty)
(check-expect (find-stress-pattern empty toy-dictionary) empty)
(check-expect (find-stress-pattern '(1 0) toy-dictionary)
              (list "actress" "awful" "billion" "smugly" "ugly"))


;; -------------------------- (C) --------------------------
;; (get-rhyme pronunciation) produces the rhyme of pronunciation,
;;   which is also itself a Pronunciation
;; get-rhyme: Pronunciation -> Pronunciation
;; Example:
(check-expect (get-rhyme '(D (IH 0) S K R (AY 1) B))
              '((AY 1) B))

(define (get-rhyme pronunciation)
  (cond [(empty? pronunciation) empty]
        [(and (cons? (first pronunciation))
              (= 1 (second (first pronunciation))))
         pronunciation]
        [else
         (get-rhyme (rest pronunciation))]))


;; (word->pronunciation word dictionary) converts word into
;;   a Pronunciation, according to dictionary
;; word->pronunciation: Str Dictionary -> Pronunciation
;; Example
(check-expect (word->pronunciation "ugly" toy-dictionary)
              '((AH 1) G L (IY 0)))
(check-expect (word->pronunciation "pigeon" cmudict)
              '(P (IH 1) JH (AH 0) N))

(define (word->pronunciation word dictionary)
  (cond [(empty? dictionary) empty]
        [(string=? (first (first dictionary)) word)
         (second (first dictionary))]
        [else
         (word->pronunciation word (rest dictionary))]))


;; (find-same-rhymes word rhyme dictionary) produces a list of
;;   strings that has a rhyme equal to rhyme, but not equal to
;;   the given word.
;; find-rhymes-of-pronunciation: Str Pronunciation Dictionary -> (listof Str)
;; Example
(check-expect (find-rhymes-of-pronunciation "ugly" '((AH 1) G L (IY 0))
                                            toy-dictionary)
              (list "smugly"))

(define (find-rhymes-of-pronunciation word rhyme dictionary)
  (cond [(empty? dictionary) empty]
        [(and (equal? rhyme (get-rhyme (second (first dictionary))))
              (not (string=? word (first (first dictionary)))))
         (cons (first (first dictionary))
               (find-rhymes-of-pronunciation word rhyme
                                             (rest dictionary)))]
        [else
         (find-rhymes-of-pronunciation word rhyme (rest dictionary))]))
         

;; (find-rhymes word dictionary) produces a list of strings that
;;   rhyme with word in dictionary.
;; find-rhymes: Str Dictionary -> (listof Str)
;; requires: word must appear in dictionary
;; Example
(check-expect (find-rhymes "ugly" toy-dictionary)
              (list "smugly"))

(define (find-rhymes word dictionary)
  (find-rhymes-of-pronunciation
   word (get-rhyme (word->pronunciation word dictionary)) dictionary))

;; Tests:
(check-expect (find-rhymes "ugly" empty) empty)
(check-expect (find-rhymes "actress" toy-dictionary) empty)
(check-expect (find-rhymes "pigeon" cmudict)
              (list "bijan" "pridgen" "religion" "smidgen"))