;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 8, Question 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (remove-dups slst) consumes slst and produces slst with
;;    all duplicates in slst removed.
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;; Example
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups '(1 1 2 2 2 3 3))
              '(1 2 3))

(define (remove-dups slst)
  (foldr (lambda (f r)
           (cond [(member? f r) r]
                 [else (cons f r)]))
         empty slst))

;; Tests
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups
               '("apple" "apple" "apples"
                 "banana" "cherry" "cherry"))
              '("apple" "apples" "banana" "cherry"))
                                     

;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ifoldr combine base lst) is an indexed version of foldr,
;;   consuming an extra argument in combine function as index.
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;; Example
(check-expect (ifoldr (lambda (i x y) (cons (list i x) y)) empty '(a b c))
              '((0 a) (1 b) (2 c)))
(check-expect (ifoldr (lambda (i f r) (cons (* i f) r)) empty '(0 1 2 3))
              '(0 1 4 9))

(define (ifoldr combine base lst)
  (local
    [;; (ifoldr/acc combine base lst acc) consumes combine, base and
     ;;   a lst, along with a acc accumulator, and proudces an
     ;;   indexed version of foldr.
     ;; ifoldr/acc: (Nat X Y -> Y) Y (listof X) Nat -> Y
     
     (define (ifoldr/acc combine base lst acc)
       (cond [(empty? lst) base]
             [else
              (combine acc (first lst)
                       (ifoldr/acc combine base
                                   (rest lst) (add1 acc)))]))]
    (ifoldr/acc combine base lst 0)))

;; Tests
(check-expect (ifoldr (lambda (i f r) (cons (+ i f) r)) empty '(0 1 2 3))
              '(0 2 4 6))


;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))


;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
;;;; (check-expect (remove-letters "abc") '("bc" "ac" "ab"))
;;;; (check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc)
                (lambda (i) (list->string (remove-at i loc))))))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (members? needles haystack) determines if ALL of the needles
;;   appear in the haystack.
;; members?: (listof Any) (listof Any) -> Bool
;; Example
(check-expect (members? '(2 4 5) '(1 2 3 4 5)) true)
(check-expect (members? empty '(1 2 3)) true)
(check-expect (members? '(1 2 3) empty) false)

(define (members? needles haystack)
  (andmap (lambda (x) (member? x haystack)) needles))

;; Tests
(check-expect (members? empty '(1 2 3)) true)
(check-expect (members? '(1 2 3) empty) false)
(check-expect (members? '(2 4 6) '(1 2 3 4 5)) false)


;; (insert-letters s) produces a list of Words formed by inserting
;;    all possible letters into s.
;; insert-letters: Word -> (listof Word)
;; Example
(check-expect (members? '("a12" "b12" "1y2" "1z2")
                        (insert-letters "12")) true)

(define (insert-letters s)
  (local [(define loc (string->list s))
          ;; (insert-at char index lst) inserts char at the index
          ;;   position in lst
          ;; insert-at: Char Nat (listof Char) -> (listof Char)
          
          (define (insert-at char index lst)
            (ifoldr (lambda (i f r)
                      (cond [(= i index)
                             (cons char (cons f r))]
                            [else
                             (cons f r)]))
                    empty lst))

          ;; (insert-before index) inserts every letter of the
          ;;   alphabet in front of each character of loc, as
          ;;   defined above.
          ;; insert-before: Nat -> (listof Word)
          
          (define (insert-before-index index)
            (build-list (length letters)
                        (lambda (i)
                          (list->string
                           (insert-at (list-ref letters i) index loc)))))]
    (ifoldr (lambda (i f r)
              (append (insert-before-index i) r))
            empty loc)))

;; Tests
(check-expect (insert-letters "") '())


;; (trailing-letters s) produces a list of Words by inserting
;;   each possible letter of the alphabet at the end of s.
;; trailing-letters: Word -> (listof Word)
;; Example
(check-expect (members? '("12a" "12b" "12y" "12z")
                        (trailing-letters "12")) true)

(define (trailing-letters s)
  (local [(define loc (string->list s))]
    (build-list (length letters)
                (lambda (i)
                  (list->string
                   (append loc (list (list-ref letters i))))))))

;; Tests
(check-expect (members? '("a" "b" "c" "x" "y" "z")
                        (trailing-letters "")) true)


;; (replace-letters s) produces a list of Words by replacing
;;   each letter of s by every possible letter of the alphabet.
;; replace-letters: Word -> (listof Word)
;; Example
(check-expect (members? '("ab" "bb" "cb" "yb" "zb"
                          "aa" "ab" "ac" "ay" "az")
                        (replace-letters "ab")) true)

(define (replace-letters s)
  (local [(define loc (string->list s))
          
          ;; (replace-at char index lst) replaces the character
          ;;   at indexth position in lst with char
          ;; replace-at: Char Nat (listof Char) -> (listof Char)
          
          (define (replace-at char index lst)
            (ifoldr (lambda (i f r)
                      (cond [(= i index)
                             (cons char r)]
                            [else
                             (cons f r)]))
                    empty lst))

          ;; (replace-letter-at index) produces a list of Words by
          ;;   replacing the indexth position of loc with every
          ;;   possible character from the alphabet
          ;; replace-letter-at: Nat -> (listof Word)
          
          (define (replace-letter-at index)
            (build-list (length letters)
                        (lambda (i)
                          (list->string
                           (replace-at (list-ref letters i)
                                       index loc)))))]
    (ifoldr (lambda (i f r)
              (append (replace-letter-at i) r))
            empty loc)))

;; Tests
(check-expect (replace-letters "") '())


;; (swap-letters s) produces a list of Words with each
;;   adjacent character of s being swapped to form the
;;   new Word. 
;; swap-letters: Word -> (listof Word)
;; Example
(check-expect (swap-letters "ab") '("ba"))
(check-expect (swap-letters "") '())

(define (swap-letters s)
  (local [(define loc (string->list s))

          ;; (swap-adjacent index lst) swaps the indexth character
          ;;   of lst with its next adjacent character of lst
          ;; swap-adjacent: Nat (listof Char) -> (listof Char)
          ;; require: lst is nonempty
          
          (define (swap-adjacent index lst)
            (ifoldr (lambda (i f r)
                      (cond [(= i index)
                             (cons (first r) (cons f (rest r)))]
                            [else
                             (cons f r)]))
                    empty lst))]
    (cond [(empty? loc) empty]
          [else
           (build-list (sub1 (length loc))
                       (lambda (i)
                         (list->string (swap-adjacent i loc))))])))

;; Tests
(check-expect (swap-letters "") '())
(check-expect (swap-letters "jklm")
              '("kjlm" "jlkm" "jkml"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (suggest s valid?) consumes a Word, s, and a predicate valid?,
;;   and produces a list of Words with exactly an edit distance
;;   of one to s, with each Word of the list having a true value
;;   while testing with valid?
;; suggest: Word (Word -> Bool) -> (listof Word)
;; Example
(define (valid? s)
  (member? s '("rights" "right" "fight"
               "aardvark" "fhqwhgads" "bright")))
(check-expect (suggest "right" valid?)
              '("bright" "fight" "rights"))

(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))

;; Tests
(check-expect (suggest "" (lambda (s) (member? s '("a" "b" "c" "j" "z"))))
              '("a" "b" "c" "j" "z"))
(check-expect (suggest "a" (lambda (s) (member? s '("aa" "ab" "ac"))))
              '("aa" "ab" "ac"))