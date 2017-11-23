;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang (20714726)
;; CS135 Fall 2017
;; Assignment 06, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct node (key val left right))
;; A Node is a (make-node Num Str BT BT)


;; A BT is one of:
;; * empty
;; * Node


;; Useful constants for examples and testing
(define exampleBT
  (make-node 1 "a"
             (make-node 7 "b" empty empty)
             (make-node 3 "c" (make-node 7 "d" empty empty) empty)))
(define root
  (make-node 1 "a" empty empty))
(define exampleBT-pruned-at-7
  (make-node 1 "a" empty 
                   (make-node 3 "c" empty empty)))


;; =========================== (A) ==============================
;; (height bt) consumes bt, which is a binary tree, and produces
;;   the height of bt.
;; height: BT -> Nat
;; Example
(check-expect (height empty) 0)
(check-expect (height exampleBT) 3)

(define (height bt)
  (cond [(empty? bt) 0]
        [else (+ 1 (max (height (node-left bt))
                        (height (node-right bt))))]))

;; Tests
(check-expect (height empty) 0)
(check-expect (height root) 1)


;; =========================== (B) ==============================
;; (find-in-tree bt los) consumes a list of symbols, los, and a
;;   binary tree, bt, and returns the key which is in the node
;;   rooted at the tree after following these movements, or false
;;   if the path goes beyond the leaf of the tree
;; find-in-tree: BT (listof (anyof 'L 'R)) -> (anyof Num false)
;; Example
(check-expect (find-in-tree exampleBT empty) 1)
(check-expect (find-in-tree exampleBT '(R L)) 7)

(define (find-in-tree bt los)
  (cond [(empty? bt) false]
        [(empty? los) (node-key bt)]
        [(symbol=? (first los) 'L)
         (find-in-tree (node-left bt) (rest los))]
        [(symbol=? (first los) 'R)
         (find-in-tree (node-right bt) (rest los))]))

;; Tests
(check-expect (find-in-tree exampleBT empty) 1)
(check-expect (find-in-tree exampleBT '(L L)) false)


;; =========================== (C) ==============================
;; (prune bt key) produces the binary tree, bt, where all subtrees
;;   rooted at key have been removed.
;; prune: BT Num -> BT
;; Example
(check-expect (prune empty 1) empty)
(check-expect (prune exampleBT 1) empty)

(define (prune bt key)
  (cond [(empty? bt) empty]
        [(= (node-key bt) key)
         empty]
        [else
         (make-node (node-key bt)
                    (node-val bt)
                    (prune (node-left bt) key)
                    (prune (node-right bt) key))]))

;; Tests
(check-expect (prune empty 1) empty)
(check-expect (prune exampleBT 7)
              exampleBT-pruned-at-7)