;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang (20714726)
;; CS135 Fall 2017
;; Assignment 06, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Useful constants for examples and testing
(define sample-table
  (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
(define sample-table-mult-by-2
  (list (list 16 6 8 18) (list 6 14 10 12) (list -2 2 -6 0)))
(define sample-table-2nd-row
  (list 3 7 5 6))
(define sample-table-2nd-column
  (list 3 7 1))
(define sample-table-3rd-column
  (list 4 5 -3))
(define sample-table2
  (list (list 2 8) (list 4 6)))
(define sample-table2-mult-by-0.5
  (list (list 1 4) (list 2 3)))
(define sample-list
  (list 1 2 3 4 5))
(define sample-list-mult-by-2
  (list 2 4 6 8 10))
(define sample-table3
  (list (list 2 2 2 2) (list 2 2 2 2) (list 2 2 2 2)))
(define sum-sample-table-1-3
  (list (list 10 5 6 11) (list 5 9 7 8) (list 1 3 -1 2)))
(define negate-sample-table
  (list (list -8 -3 -4 -9) (list -3 -7 -5 -6) (list 1 -1 3 0)))
(define zero-table
  (list (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)))


;; ======================= (A) ==============================
;; (mult-by-list number list) produces the list resulting
;;   from each number of the list being multiplied by number
;; mult-by-list: Num (listof Num) -> (listof Num)
;; Example
(check-expect (mult-by-list 2 sample-list)
              sample-list-mult-by-2)

(define (mult-by-list number list)
  (cond [(empty? list) empty]
        [else
         (cons (* number (first list))
               (mult-by-list number (rest list)))]))


;; (mult-by number table) produces the table resulting from
;;   each number being multiplied by the consumed number
;; mult-by: Num Table -> Table
;; Example
(check-expect (mult-by 2 sample-table)
              sample-table-mult-by-2)
(check-expect (mult-by 3 empty) empty)
(check-expect (mult-by 3 (list empty)) (list empty))

(define (mult-by number table)
  (cond [(empty? table) empty]
        [else
         (cons (mult-by-list number (first table))
                 (mult-by number (rest table)))]))

;; Tests:
(check-expect (mult-by 3 empty) empty)
(check-expect (mult-by 3 (list empty)) (list empty))
(check-expect (mult-by 0.5 sample-table2)
              sample-table2-mult-by-0.5)

;; ======================= (B) ==============================
;; (get-num-rows n table) produces the number of rows in table,
;;   using n as an intermediate storage for current number of
;;   rows
;; get-num-rows: Nat Table -> Nat
;; Example
(check-expect (get-num-rows 0 sample-table) 3)

(define (get-num-rows n table)
  (cond [(empty? table) n]
        [else
         (get-num-rows (add1 n) (rest table))]))


;; (get-num-columns n list) produces the number of columns in list,
;;   using n as an intermediate storage for current number of
;;   columns
;; get-num-columns: Nat (listof Any) -> Nat
;; Example
(check-expect (get-num-columns 0 (first sample-table)) 4)

(define (get-num-columns n list)
  (cond [(empty? list) n]
        [else
         (get-num-columns (add1 n) (rest list))]))


;; (get-row n r table) produces the rth row in the table, using
;;   n as the current index of the row visiting
;; get-row: Nat Nat Table -> (listof Num)
;; Example
(check-expect (get-row 0 1 sample-table)
              sample-table-2nd-row)

(define (get-row n r table)
  (cond [(empty? table) empty]
        [(> n r) empty]
        [(= n r) (first table)]
        [else
         (get-row (add1 n) r (rest table))]))

;; (get-column n c list) produces the cth element in the
;;   list, using n as the current index of the column
;;   visiting
;; get-column: Nat Nat (listof Any) -> Any
;; Example
(check-expect (get-column 0 2 (first sample-table)) 4)

(define (get-column n c list)
  (cond [(empty? list) empty]
        [(> n c) empty]
        [(= n c) (first list)]
        [else
         (get-column (add1 n) c (rest list))]))


;; (get-elem row column table) produces the number which
;;   is in that row and column in table.
;; get-elem: Nat Nat Table -> (anyof Num false)
;; Example
(check-expect (get-elem 0 0 empty) false)
(check-expect (get-elem 7 0 sample-table) false)
(check-expect (get-elem 0 8 sample-table) false)
(check-expect (get-elem 0 0 sample-table) 8)

(define (get-elem row column table)
  (cond [(or (empty? table)
             (> row (sub1 (get-num-rows 0 table)))
             (> column (sub1 (get-num-columns 0 (first table)))))
         false]
        [else
         (get-column 0 column (get-row 0 row table))]))

;; Tests
(check-expect (get-elem 0 0 empty) false)
(check-expect (get-elem 3 0 sample-table) false)
(check-expect (get-elem 0 4 sample-table) false)
(check-expect (get-elem 2 3 sample-table) 0)


;; ======================= (C) ==============================
;; (col column-number table) produces a list of all numbers
;;   in the column indexed at column-number in table.
;; col: Nat Table -> (listof Num)
;; Example
(check-expect (col 2 empty) empty)
(check-expect (col 5 sample-table) empty)
(check-expect (col 2 sample-table) sample-table-3rd-column)

(define (col column-number table)
  (cond [(empty? table) empty]
        [(empty? (get-column 0 column-number (first table))) empty]
        [else
         (cons (get-column 0 column-number (first table))
               (col column-number (rest table)))]))

;; Tests
(check-expect (col 2 empty) empty)
(check-expect (col 5 sample-table) empty)
(check-expect (col 1 sample-table) sample-table-2nd-column)


;; ======================= (D) ==============================
;; (sum-list list1 list2) produces a single list which results
;;   from adding up the elements pairwise between list1 and list2
;; sum-list: (listof Num) (listof Num) -> (listof Num)
;; requires: list1, list2 have equal length.
;; Example
(check-expect (sum-list (list 1 2) (list 100 200))
              (list 101 202))

(define (sum-list list1 list2)
  (cond [(empty? list1) empty]
        [else
         (cons (+ (first list1) (first list2))
               (sum-list (rest list1) (rest list2)))]))


;; (sum-tables table1 table2) produces a single table which
;;   results from adding up the elements pairwise between
;;   table1 and table2.
;; sum-tables: Table Table -> Table
;; requires: table1, table2 have same dimensions
;; Example
(check-expect (sum-tables empty empty) empty)
(check-expect (sum-tables sample-table sample-table3)
              sum-sample-table-1-3)

(define (sum-tables table1 table2)
  (cond [(and (empty? table1)
              (empty? table2)) empty]
        [else
         (cons (sum-list (first table1) (first table2))
               (sum-tables (rest table1) (rest table2)))]))

;; Tests
(check-expect (sum-tables empty empty) empty)
(check-expect (sum-tables sample-table negate-sample-table)
              zero-table)