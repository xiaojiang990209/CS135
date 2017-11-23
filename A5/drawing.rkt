;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang (20714726)
;; CS135 Fall 2017
;; Assignment 05, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the data definitions and functions in
;; the file drawinglib.rkt.
(require "drawinglib.rkt")


;; Useful constants for the code
(define tile-width-height 10)
(define checkerboard-square-length 10)
(define checkerboard-circle-radius 5)
(define colour-red '(255 0 0))
(define colour-white '(255 255 255))


;; Useful constants for examples and testing
(define samplepic (list
                   (make-square135 (make-posn 0 0) 50 '(255 0 0))
                   (make-square135 (make-posn 50 50) 50 '(0 0 255))
                   (make-circle135 (make-posn 50 50) 25 '(0 255 0))))
(define samplepic-with-circle-removed
  (list (make-square135 (make-posn 0 0) 50 '(255 0 0))
        (make-square135 (make-posn 50 50) 50 '(0 0 255))))
(define samplepic-with-one-square-removed
  (list (make-square135 (make-posn 0 0) 50 '(255 0 0))
        (make-circle135 (make-posn 50 50) 25 '(0 255 0))))
(define samplepic-with-all-squares-removed
  (list (make-circle135 (make-posn 50 50) 25 '(0 255 0))))
(define samplepic2 (list
                    (make-circle135 (make-posn 25 25) 25 '(255 0 0))
                    (make-square135 (make-posn 50 50) 50 '(0 0 255))
                    (make-circle135 (make-posn 50 50) 25 '(0 255 0))))
(define samplepic2-with-only-one-circle
  (list
   (make-circle135 (make-posn 25 25) 25 '(255 0 0))
   (make-square135 (make-posn 50 50) 50 '(0 0 255))))
(define red-ring-with-radius-10
  (make-circle135 (make-posn 100 100) 10 colour-red))
(define four-discs-with-radius-25
  (list
   (make-circle135 (make-posn 100 100) 100 colour-red)
   (make-circle135 (make-posn 100 100) 75 colour-white)
   (make-circle135 (make-posn 100 100) 50 colour-red)
   (make-circle135 (make-posn 100 100) 25 colour-white)))
   

;; -------------------------- (A) --------------------------
;; An example drawing
(define example-drawing
  (list (make-square135 (make-posn 0 0) 100 '(255 0 0))
        (make-square135 (make-posn 100 0) 100 '(0 255 0))
        (make-square135 (make-posn 200 0) 100 '(0 0 255))
        (make-circle135 (make-posn 100 200) 25 '(0 0 0))
        (make-circle135 (make-posn 150 200) 25 '(0 0 0))
        (make-circle135 (make-posn 200 200) 25 '(0 0 0))))


;; -------------------------- (B) --------------------------
;; (calc-num-squares drawing) produces the number of squares
;;   in drawing.
;; calc-num-squares: Drawing -> Nat
;; Example
(check-expect (calc-num-squares samplepic) 2)

(define (calc-num-squares drawing)
  (cond [(empty? drawing) 0]
        [(square135? (first drawing))
         (+ 1 (calc-num-squares (rest drawing)))]
        [else
         (calc-num-squares (rest drawing))]))


;; (calc-num-circles drawing) produces the number of circles
;;   in drawing.
;; calc-num-circles: Drawing -> Nat
;; Example
(check-expect (calc-num-circles samplepic) 1)

(define (calc-num-circles drawing)
  (cond [(empty? drawing) 0]
        [(circle135? (first drawing))
         (+ 1 (calc-num-circles (rest drawing)))]
        [else
         (calc-num-circles (rest drawing))]))


;; (cull-keep-squares drawing n) produces a new drawing
;;   containing all the squares and the first n circles
;;   in drawing.
;; cull-keep-squares: Drawing Nat -> Drawing
;; Example
(check-expect (cull-keep-squares samplepic 0)
              samplepic-with-circle-removed)
(check-expect (cull-keep-squares samplepic2 1)
              samplepic2-with-only-one-circle)

(define (cull-keep-squares drawing n)
  (cond [(empty? drawing) empty]
        [(and (circle135? (first drawing)) (not (zero? n)))
         (cons (first drawing)
               (cull-keep-squares (rest drawing) (sub1 n)))]
        [(circle135? (first drawing))
         (cull-keep-squares (rest drawing) n)]
        [else
         (cons (first drawing)
               (cull-keep-squares (rest drawing) n))]))


;; (cull-keep-circles drawing m) produces a new drawing
;;   containing all the circles and the first m squares
;;   in drawing.
;; cull-keep-circles: Drawing Nat -> Drawing
;; Example
(check-expect (cull-keep-circles samplepic 1)
              samplepic-with-one-square-removed)

(define (cull-keep-circles drawing m)
  (cond [(empty? drawing) empty]
        [(and (square135? (first drawing)) (not (zero? m)))
         (cons (first drawing)
               (cull-keep-circles (rest drawing) (sub1 m)))]
        [(square135? (first drawing))
         (cull-keep-circles (rest drawing) m)]
        [else
         (cons (first drawing)
               (cull-keep-circles (rest drawing) m))]))


;; (normal-cull drawing m n) produces a new drawing
;;   containing the first m squares and the first n
;;   circles of drawing.
;; normal-cull: Drawing Nat Nat -> Drawing
;; requires: m <= (calc-num-squares drawing)
;;           n <= (calc-num-circles drawing)
;; Example
(check-expect (normal-cull samplepic 1 1)
              samplepic-with-one-square-removed)

(define (normal-cull drawing m n)
  (cond [(empty? drawing) empty]
        [(and (zero? m) (zero? n)) empty]
        [(and (square135? (first drawing))
              (not (zero? m)))
         (cons (first drawing) (normal-cull (rest drawing) (sub1 m) n))]
        [(and (circle135? (first drawing))
              (not (zero? n)))
         (cons (first drawing) (normal-cull (rest drawing) m (sub1 n)))]
        [else
         (normal-cull (rest drawing) m n)]))


;; (cull drawing m n) produces a new drawing containing the
;;   first m squares and the first n circles in the original
;;   drawing
;; cull: Drawing Nat Nat -> Drawing
;; Example
(check-expect (cull samplepic 1 1) samplepic-with-one-square-removed)

(define (cull drawing m n)
  (cond [(and (> m (calc-num-squares drawing))
              (> n (calc-num-circles drawing)))
         drawing]
        [(> m (calc-num-squares drawing))
         (cull-keep-squares drawing n)]
        [(> n (calc-num-circles drawing))
         (cull-keep-circles drawing m)]
        [else
         (normal-cull drawing m n)]))

;; Tests
(check-expect (cull samplepic 0 0) empty)
(check-expect (cull samplepic 5 0) samplepic-with-circle-removed)
(check-expect (cull samplepic 0 5) samplepic-with-all-squares-removed)
(check-expect (cull samplepic 5 5) samplepic)
(check-expect (cull samplepic2 1 1) samplepic2-with-only-one-circle)


;; -------------------------- (C) --------------------------
;; (compute-ring radius red?) produces a Circle135 with the given
;;   radius, centered at (100,100), with colour equal to red if
;;   red? is true, and white if red? is false.
;; compute-ring: Num Bool -> Circle135
;; requires: radius > 0
;; Example
(check-expect (compute-ring 10 true)
              red-ring-with-radius-10)

(define (compute-ring radius red?)
  (make-circle135
   (make-posn 100 100)
   radius
   (cond [red? colour-red]
         [else colour-white])))


;; (compute-rings width n) produces a Drawing containing n 
;;   alternating red and white discs centered on (100,100),
;;   each disc with a width of the given width.
;; compute-rings: Num Nat -> Drawing
;; Example
(check-expect (compute-rings 25 4)
              four-discs-with-radius-25)

(define (compute-rings width n)
  (cond [(zero? n) empty]
        [else
         (cons (compute-ring (* n width) (even? n))
               (compute-rings width (sub1 n)))]))


;; (bullseye n) produces a Drawing containing n alternating
;;   red and white discs centered on (100,100), each disc
;;   of equal width.
;; bullseye: Nat -> Drawing
;; Example
(check-expect (bullseye 4) four-discs-with-radius-25)

(define (bullseye n)
  (compute-rings (/ 100 n) n))


;; -------------------------- (D) --------------------------
;; (compute-square row column colour) computes a Square135
;;   according to the given row, column and colour
;; compute-square: Nat Nat Colour -> Square135
;; Example
(check-expect (compute-square 0 0 colour-white)
              (make-square135 (make-posn 0 0)
                              checkerboard-square-length
                              colour-white))

(define (compute-square row column colour)
  (make-square135
   (make-posn (* tile-width-height column)
              (* tile-width-height row))
   tile-width-height
   colour))


;; (compute-circle row column colour) computes a Circle135
;;   according to the given row, column and colour
;; compute-circle: Nat Nat Colour -> Circle135
;; Example
(check-expect (compute-circle 0 0 colour-red)
              (make-circle135 (make-posn 5 5)
                              checkerboard-circle-radius
                              colour-red))

(define (compute-circle row column colour)
  (make-circle135
   (make-posn (+ checkerboard-circle-radius
                 (* tile-width-height column))
              (+ checkerboard-circle-radius
                 (* tile-width-height row)))
   checkerboard-circle-radius
   colour))


;; (compute-columns column row num-column c1 c2) computes the
;;   columns of shapes of the checkerboard on the current row,
;;   according to the given column, row, num-column, c1 and c2.
;; compute-columns: Nat Nat Nat Colour Colour -> Drawing

(define (compute-columns column row num-column c1 c2)
  (cond [(>= column num-column) empty]
        [(and (even? row) (even? column))
         (cons (compute-square row column c1)
               (compute-columns
                (add1 column) row num-column c1 c2))]
        [(and (even? row) (odd? column))
         (cons (compute-circle row column c2)
               (compute-columns
                (add1 column) row num-column c1 c2))]
        [(and (odd? row) (even? column))
         (cons (compute-circle row column c2)
               (compute-columns
                (add1 column) row num-column c1 c2))]
        [else
         (cons (compute-square row column c1)
               (compute-columns
                (add1 column) row num-column c1 c2))]))


;; (compute-row row num-column c1 c2) is a wrapper function
;;   of compute-columns, computes the current row of the
;;   checkerboard using the given row, num-column, c1 and c2.
;; compute-row: Nat Nat Colour Colour -> Drawing

(define (compute-row row num-column c1 c2)
  (compute-columns 0 row num-column c1 c2))


;; (rows-from row num-row num-column c1 c2) computes the complete
;;   checkerboard drawing by computing each individual row based on
;;   row, num-row, num-column, c1 and c2.
;; rows-from: Nat Nat Nat Colour Colour -> Drawing

(define (rows-from row num-row num-column c1 c2)
  (cond [(>= row num-row) empty]
        [else
         (append (compute-row row num-column c1 c2)
                 (rows-from (add1 row) num-row num-column c1 c2))]))


;; (checkerboard n c1 c2) computes the complete checkerboard
;;   drawing based on n, c1 and c2, and it is a wrapper function
;;   of rows-from.
;; checkerboard: Nat Colour Colour -> Drawing

(define (checkerboard n c1 c2)
  (rows-from 0 n n c1 c2))