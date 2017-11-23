;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 8, Question 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Useful constants for examples and tests
(define list1 (list 'a 3 4 4.5))
(define list2 (list 'a 'b 'c))
(define list3 (list 3 -4 4.5 4.7 -1))
(define list4 (list -1 -2 -3 -4))
(define id-al
  (list (list 1 "Tom") (list 4 "Jake") (list 789 "Kevin")))
(define birth-date-al
  (list (list 1989 "Tom") (list 1992 "Lola") (list 1999 "Roger")))


;; =============== (a) ===================
;; (keep-ints lst) consumes lst and produces a list with
;;   only the integers in lst in it.
;; keep-ints: (listof Any) -> (listof Int)
;; Example
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints list1) (list 3 4))

(define (keep-ints lst)
  (filter integer? lst))

;; Tests
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints list2) empty)


;; ================ (b) ====================
;; (contains? elem lst) produces true if elem is in lst,
;;   and false otherwise
;; contains?: Any (listof Any) -> Bool
;; Example
(check-expect (contains? 4 list1) true)
(check-expect (contains? 4 empty) false)

(define (contains? elem lst)
  (foldr (lambda (f r)
           (cond [(equal? f elem) true]
                 [else r]))
           false lst))

;; Tests
(check-expect (contains? 'a empty) false)
(check-expect (contains? 'b list2) true)


;; ================ (c) ====================
;; (lookup-al key alst) produces the value corresponding to
;;   key, or false if key is not present
;; lookup-al: Num AL -> (anyof Str false)
;; Example
(check-expect (lookup-al 4 id-al) "Jake")
(check-expect (lookup-al 3 id-al) false)
(check-expect (lookup-al 2 empty) false)

(define (lookup-al key alst)
  (foldr (lambda (f r)
           (cond [(= (first f) key)
                  (second f)]
                 [else r]))
         false alst))

;; Tests
(check-expect (lookup-al 789 id-al) "Kevin")
(check-expect (lookup-al 6 id-al) false)
(check-expect (lookup-al 6 empty) false)


;; ==================== (d) ========================
;; (extract-keys alst) consumes alst(AL) and produces a list
;;   of all of the keys in alst
;; extract-keys: AL -> (listof Num)
;; Examples
(check-expect (extract-keys empty) empty)
(check-expect (extract-keys id-al) (list 1 4 789))

(define (extract-keys alst)
  (foldr (lambda (f r)
           (cons (first f) r))
         empty alst))

;; Tests
(check-expect (extract-keys empty) empty)
(check-expect (extract-keys birth-date-al) (list 1989 1992 1999))


;; ==================== (e) =======================
;; (sum-positive lst) consumes lst and produces the sum
;;   of all positive numbers in lst
;; sum-positive: (listof Num) -> Num
;; Example
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive list3) 12.2)

(define (sum-positive lst)
  (foldr (lambda (f r)
           (cond [(> f 0)
                  (+ f r)]
                 [else r]))
         0 lst))

;; Tests
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive list4) 0)


;; ===================== (f) =========================
;; (countup-to n b) produces a list from n....b
;; countup-to: Int Int -> (listof Int)
;; requires: n <= b
;; Example
(check-expect (countup-to 6 8) (list 6 7 8))
(check-expect (countup-to 6 6) (list 6))

(define (countup-to n b)
  (filter (lambda (f)
            (>= f n))
          (build-list (add1 b) (lambda (i) i))))

;; Tests
(check-expect (countup-to 999 1002) (list 999 1000 1001 1002))
(check-expect (countup-to 100 100) (list 100))


;; ==================== (g) =========================
;; (shout los) consumes a list of strings, los, and produces
;;   the same list of strings, but all in UPPERCASE
;; shout: (listof Str) -> (listof Str)
;; Example
(check-expect (shout (list "get" "off" "my" "lawn"))
              (list "GET" "OFF" "MY" "LAWN"))
(check-expect (shout empty) empty)

(define (shout los)
  (map (lambda (s)
         (list->string
          (map char-upcase (string->list s)))) los))

;; Tests
(check-expect (shout (list "i" "love" "you"))
              (list "I" "LOVE" "YOU"))
(check-expect (shout empty) empty)


;; ==================== (h) ==========================
;; (make-validator lst) consumes lst (listof Any) and produces
;;   a predicate function. The produced function consumes an item
;;   of type Any and produces a Boolean that determines if the item
;;   appears in the list that was consumed by make-validator
;; make-validator: (listof Any) -> (Any -> Bool)
;; Example
(check-expect ((make-validator list1) 4) true)
(check-expect ((make-validator empty) 0) false)

(define (make-validator lst)
  (lambda (elem)
    (contains? elem lst)))

;; Tests
(check-expect ((make-validator empty) 0) false)
(check-expect ((make-validator list2) 'b) true)
(check-expect ((make-validator list2) 'd) false)