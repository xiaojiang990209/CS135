;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname moretabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 07, Question 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length


;; Useful constants for examples and testing
(define sample-table '((7 4.5 -3.2) (-3 3 13)))
(define sample-table2 '((7 4.5 3.2) (-3 3 13)))
(define sample-list-function
  (list abs floor add1))
(define sample-table-function-application
  (list (list (list 7 4.5 3.2)
              (list 3 3 13))
        (list (list 7 4 -4)
              (list -3 3 13))
        (list (list 8 5.5 -2.2)
              (list -2 4 14))))


;; Global helper functions
;; (apply-function f arg) produces the result of f with the given
;;   argument arg.
;; apply-function: (X->Y) X -> Y

(define (apply-function f arg)
  (f arg))


;; =============== (A) =================
;; (mirror table) consumes table and reverses the elements of 
;;   each row.
;; mirror: Table -> Table
;; Examples
(check-expect (mirror '((-3.2 4.5 7) (13 3 -3)))
              (list (list 7 4.5 -3.2) (list -3 3 13)))
(check-expect (mirror empty) empty)

(define (mirror table)
  (local
    [;; (mirror-row/acc row reverse) consumes row and the
     ;;   accumulator reverse, and produces the reversed
     ;;   row
     ;; mirror-row/acc: (listof Num) (listof Num) -> (listof Num)
     
     (define (mirror-row/acc row reverse)
       (cond [(empty? row) reverse]
             [else 
              (mirror-row/acc (rest row)
                              (cons (first row) reverse))]))

     ;; (mirror-table table) consumes table and produces
     ;;   the same table with each row of the table in
     ;;   reverse order
     ;; mirror-table: Table -> Table
     
     (define (mirror-table table)
       (cond [(empty? table) empty]
             [else
              (cons (mirror-row/acc (first table) empty)
                    (mirror-table (rest table)))]))]
    
    (mirror-table table))    
  )

;; Tests
(check-expect (mirror '((-3.2 4.5 7 100) (80 13 3 -3)))
              (list (list 100 7 4.5 -3.2) (list -3 3 13 80)))
(check-expect (mirror empty) empty)


;; ====================== (B) =========================
;; (element-apply-many lof table) consumes lof, a list of functins,
;;   and table, and produces a list of tables.
;; element-apply-many: (listof (anyof (Num -> Num) (Num -> Int) 
;;                     (Num -> Nat)) Table -> (listof Table)
;; Example
(check-expect (element-apply-many empty sample-table) empty)
(check-expect (element-apply-many sample-list-function empty)
              (list empty empty empty))
(check-expect (element-apply-many sample-list-function sample-table)
              sample-table-function-application)

(define (element-apply-many lof table)
  (local
    [;; (apply-row function row) consumes function and row, and
     ;;   apply the function to each number in row.
     ;; apply-row: (anyof (Num -> Num) (Num -> Int) (Num -> Nat))
     ;;            -> (listof Num)
     
     (define (apply-row function row)
       (cond [(empty? row) empty]
             [else
              (cons (function (first row))
                    (apply-row function (rest row)))]))

     ;; (apply-table function table) consumes function and table, and
     ;;   apply the function to each number in table.
     ;; apply-table: (anyof (Num -> Num) (Num -> Int) (Num -> Nat))
     ;;              -> Table
     
     (define (apply-table function table)
       (cond [(empty? table) empty]
             [else
              (cons (apply-row function (first table))
                    (apply-table function (rest table)))]))]
    
    (cond [(empty? lof) empty]
          [else
           (cons (apply-table (first lof) table)
                 (element-apply-many (rest lof) table))])))

;; Tests
(check-expect (element-apply-many empty sample-table) empty)
(check-expect (element-apply-many sample-list-function empty)
              (list empty empty empty))
(check-expect (element-apply-many sample-list-function sample-table)
              sample-table-function-application)


;; =================== (C) =======================
;; (scale-smallest table offset) consumes table and offset, and
;;   produces a second function that consumes a number, multiplies
;;   that number by the smallest element of the table, and adds
;;   the offset.
;; scale-smallest: Table Num -> (Num -> Num)
;; requires: table is nonempty
;; Examples
(check-expect (apply-function (scale-smallest sample-table2 2.4) 7)
              -18.6)
(check-expect (apply-function (scale-smallest sample-table2 2.4) 0)
              2.4)
(check-expect (apply-function (scale-smallest '((0)) 5) 7) 5)

(define (scale-smallest table offset)
  (local
    [;; (row-min/acc row min-so-far) consumes row, the current row, and
     ;;   and the accumulator min-so-far, and computes the minimum entry
     ;;   of the rowth row of table
     ;; row-min/acc: (listof Num) Num -> Num
     
     (define (row-min/acc row min-so-far)
       (cond [(empty? row) min-so-far]
             [(< (first row) min-so-far)
              (row-min/acc (rest row) (first row))]
             [else 
              (row-min/acc (rest row) min-so-far)]))

     ;; (table-min.acc table min-so-far) consumes table and the
     ;;   accumulator min-so-far, and computes the minimum entry of
     ;;   the entire table
     ;; table-min/acc: Table Num -> Num
     
     (define (table-min/acc table min-so-far)
       (cond [(empty? table) min-so-far]
             [else
              (table-min/acc (rest table)
                             (local [(define row-min
                                       (row-min/acc (first table) (first (first table))))]
                               (cond [(< row-min min-so-far) row-min]
                                     [else min-so-far])))]))

     ;; (output-function num) consumes num and produces a function
     ;;   that consumes a number and produces a number
     ;; output-function: Num -> (Num -> Num)
     
     (define (output-function num)
       (+ (* num (table-min/acc table (first (first table)))) offset))]
    output-function))

;; Tests
(check-expect (apply-function (scale-smallest sample-table2 2.4) -2.7)
              10.5)
(check-expect (apply-function (scale-smallest sample-table2 0) 0) 0)
(check-expect (apply-function (scale-smallest sample-table 16) 5) 0)
(check-expect (apply-function (scale-smallest '((0)) 5) 7) 5)