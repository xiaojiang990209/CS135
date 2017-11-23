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

(define (contains-used-row? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))]
  (foldr (lambda (flst rlst)
           (local [(define partial-result
                     (foldr (lambda (f r)
                              (cond [(false? (cell-used? f)) r]
                                    [else true])) false flst))]
             (cond [(false? partial-result) rlst]
                   [else true])))
         false rectangle)))

(define (contains-one-number? rec)
  (local [(define rectangle (rect-in-grid rect grid))]
  (foldr (lambda (flst rlst)
           (local [(define partial-result
                     (foldr (lambda (f r)
                              (cond [(and (not (zero? f)) r) false]
                                    [else r])) flst))]
             (cond [(false? partial-result) false]
                   [else rlst]))))))

(define (valid-rect? rect grid)
  (local [(define rectangle (rect-in-grid rect grid))
          (define (contains-used-cell? rec)
            (foldr (lambda (flst rlst)
                     (local [(define partial-result
                               (foldr (lambda (f r)
                                        (cond [(false? (cell-used? f)) r]
                                              [else true])) flst))]
                       (cond [(false? partial-result) rlst]
                             [else true])))
                   false rectangle))
          (define (contains-one-number? rec)
            (foldr (lambda (flst rlst)
                     (local [(define partial-result
                               (foldr (lambda (f r)
                                        (cond [(and (not (zero? f)) r) false]
                                              [else r])) flst))]
                       (cond [(false? partial-result) false]
                             [else rlst]))) false rectangle))] 
                              

           empty))
    

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