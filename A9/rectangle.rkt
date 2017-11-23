;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang  (20714726)
;; CS135 Fall 2017
;; Assignment 09, Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))


;; Here are a couple of constants that can be used to define
;; the puzzle in the assignment, and a random larger puzzle.

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))

(define first-unused-at-2-1
  (make-state
   (list (list
          (make-cell 0 true)
          (make-cell 1 true)
          (make-cell 2 true))
         (list
          (make-cell 3 true)
          (make-cell 4 true)
          (make-cell 5 false))
         (list
          (make-cell 6 false)
          (make-cell 7 false)
          (make-cell 8 false))) empty))

(define state1
  (make-state
   (list (list
          (make-cell 0 true)
          (make-cell 0 true)
          (make-cell 2 true))
         (list
          (make-cell 0 true)
          (make-cell 4 true)
          (make-cell 5 false))
         (list
          (make-cell 6 false)
          (make-cell 7 false)
          (make-cell 8 false))) empty))


;; ====================== (A) ==========================
;; (map2d f nested-lst) consumes a function f and a list of lists
;;   of values nested-lst, and produces a new list of lists in which
;;   f has been applied to every element of the input.
;; map2d: (X > Y) (listof (listof X)) -> (listof (listof Y))

(define (map2d f nested-lst)
  (map (lambda (lst) (map (lambda (elem) (f elem)) lst)) nested-lst))


;; ====================== (b) ===========================
;; (construct-puzzle nested-lst) consumes nested-lst and produces
;;   a State representing the initial state of a puzzle
;; construct-puzzle: (listof (listof Nat)) -> State
;; Examples

(define (construct-puzzle nested-lst)
  (make-state (map2d (lambda (elem) (make-cell elem false)) nested-lst)
              empty))


;; ====================== (c) ===========================
;; (solved? state) consumes state and produces a boolean value
;;   that indicates whether the puzzle described by state is
;;   fully solved
;; solved?: State -> Bool
;; Example

(define (solved? state)
  (foldr (lambda (flst rlst)
           (local [(define row-result
                     (foldr (lambda (f r)
                              (cond [(false? (cell-used? f)) false]
                                    [else r]))
                            true flst))]
             (cond [(false? row-result) false]
                   [else rlst])))
           true (state-grid state)))


;; ====================== (d) ===========================
;; (get-first-unused grid) consumes grid and finds the topmost,
;;   leftmost cell in the grid that isn't marked as used.
;; get-first-unused: Grid -> (list Nat Nat)
;; requires: grid has at least one unused cell

(define (get-first-unused grid)
  (local [(define (find-unused-row/acc row x)
            (cond [(empty? row) false]
                  [(not (cell-used? (first row))) x]
                  [else
                   (find-unused-row/acc (rest row) (add1 x))]))
          (define (find-unused-grid/acc grid y)
            (local [(define row-result (find-unused-row/acc (first grid) 0))]
              (cond [(false? row-result)
                     (find-unused-grid/acc (rest grid) (add1 y))]
                    [else
                     (list row-result y)])))]
    (find-unused-grid/acc grid 0)))


;; ====================== (e) ===========================
;; (neighbours state) consumes state and produces a list of
;;   new states that might legitimately follow from the given
;;   state after adding a single rectangle.
;; neighbours: State -> (listof State)

;; rect contains x y w h

(define (rect-in-grid rect grid)
            (local [(define rect_x (rect-x rect))
                    (define rect_y (rect-y rect))
                    (define rect_w (rect-w rect))
                    (define rect_h (rect-h rect))
                    (define (rect-in-grid-row/acc row x)
                      (cond [(= x (+ rect_x rect_w))
                             empty]
                            [(and (>= x rect_x)
                                  (< x (+ rect_x rect_w)))
                             (cons (first row)
                                   (rect-in-grid-row/acc (rest row) (add1 x)))]
                            [else
                             (rect-in-grid-row/acc (rest row) (add1 x))]))
                    (define (rect-in-grid/acc grid y)
                      (cond [(= y (+ rect_y rect_h)) empty]
                            [(and (>= y rect_y)
                                  (< y (+ rect_y rect_w)))
                             (cons (rect-in-grid-row/acc (first grid) 0)
                                   (rect-in-grid/acc (rest grid) (add1 y)))]
                            [else
                             (rect-in-grid/acc (rest grid) (add1 y))]))]
              (rect-in-grid/acc grid 0)))

(define (contains-used-cell? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))
          (define (contains-used-cell/row? row)
            (foldr (lambda (f r) (cond [(cell-used? f) true]
                                       [else r])) false row))]
  (foldr (lambda (flst rlst)
           (cond [(contains-used-cell/row? flst) true]
                 [else rlst]))
         false rectangle)))

(define (contains-one-number? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))
          (define (count-non-zero-number-row row)
            (length (filter (lambda (x) (not (zero? (cell-num x)))) row)))
          (define (count-non-zero-number-rect rect)
            (foldr (lambda (flst rlst)
                     (+ (count-non-zero-number-row flst) rlst)) 0 rect))]
    (= 1 (count-non-zero-number-rect rectangle))))

(define (find-only-num rect grid)
  (local [(define rectangle (rect-in-grid rect grid))]
    (foldr (lambda (flst rlst)
             (cons (filter (lambda (x) (not (zero? (cell-num x)))) flst) rlst))
           empty rectangle)))

(define (num-equal-area? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))
          (define only-num
            (cell-num
             (first (first
              (filter cons?
                      (foldr (lambda (flst rlst)
                               (cons (filter (lambda (x)
                                               (not (zero? (cell-num x))))
                                             flst) rlst))
                             empty rectangle))))))
          (define area (* (rect-w rect) (rect-h rect)))]
    (= only-num area)))
                     



(define (valid-rect? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))]
    (and (not (contains-used-cell rect grid))
         (contains-one-number? rect grid)
         (num-equal-area? rect grid))))
          

    

;;(define (valid-rec? rect grid)
;;  (local [
;;          (define (contains-used-row? rect row)
;;            (cond [(
;;          (define (contains-used? rect grid)
;;            empty)
; ;         (define (contains-more-than-one-number? rect grid)
;;            empty)

;;(rect-in-grid (make-rect 0 0 2 2) (state-grid first-unused-at-2-1))
(define (neighbours state)
  empty)