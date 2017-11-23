;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 8, Question 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ===================== (a) ===========================
;; (nfoldr combine1 combine2 base lst) consumes two combine
;;   functions, combine1 and combine2, the base case (base)
;;   and lst, and applies combine1 when the first element
;;   in lst is of type X, and combine2 is applied when the first
;;   element is a list.
;; nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;; Example
(define (count-items nln) (nfoldr (lambda (x y) (add1 y)) + 0 nln))
(check-expect (count-items '(1 (2 3) () ((4)))) 4)
(check-expect (count-items empty) 0)

(define (nfoldr combine1 combine2 base lst)
  (foldr (lambda (f r)
           (cond [(cons? f)
                  (combine2 (nfoldr combine1 combine2 base f)
                            r)]
                 [(empty? f) (combine2 base r)]
                 [else
                  (combine1 f r)]))
         base lst))

;; Tests
(define (flatten lst) (nfoldr cons append empty lst))
(check-expect (flatten '(1 (2 3) () ((4)))) (list 1 2 3 4))
(check-expect (flatten empty) empty)


;; ===================== (b) ===========================
;; (nfilter pred? nln) consumes pred? and nln, and produces
;;   nln with all elements that conform with pred?
;; nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;; Example
(check-expect (nfilter odd? empty) empty)
(check-expect (nfilter odd? '(1 (2 3) () ((4))))
              '(1 (3) () (())))

(define (nfilter pred? nln)
  (nfoldr (lambda (f r)
            (cond [(pred? f) (cons f r)]
                  [else r]))
          cons empty nln))

;; Tests
(check-expect (nfilter even? empty) empty)
(check-expect (nfilter zero? '(0 (1 0) (3 (0))))
              '(0 (0) ((0))))


;; ===================== (c) ===========================
;; (nmap fn nln) consumes a function fn, and a Nested list
;;   nln, and produces a nested list with all elements being
;;   applied by the function fn
;; nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;; Example
(check-expect (nmap sqr empty) empty)
(check-expect (nmap sqr '(1 (2 3) () ((4))))
              '(1 (4 9) () ((16))))

(define (nmap fn nln)
  (nfoldr (lambda (f r)
            (cons (fn f) r))
            cons empty nln))

;; Tests
(check-expect (nmap sqr empty) empty)
(check-expect (nmap add1 '(1 (2 3) (4 (5))))
              '(2 (3 4) (5 (6))))


;; ===================== (d) ===========================
;; (nreverse nln) consumes a nested list nln, and produces
;;   the nln in reverse order
;; nreverse: Nested-Listof-X -> Nested-Listof-X
;; Example
(check-expect (nreverse empty) empty)
(check-expect (nreverse '(1 (2 3) () ((4))))
              '(((4)) () (3 2) 1)) 

(define (nreverse nln)
  (nfoldr (lambda (f r)
            (append r (list f)))
          (lambda (f r)
            (append r (list f)))
          empty nln))

;; Tests
(check-expect (nreverse empty) empty)
(check-expect (nreverse '((1 (2 3)) 4 (5 (6 7 8) 9)))
              '((9 (8 7 6) 5) 4 ((3 2) 1)))


;; ===================== (e) ===========================
;; (nheight nln) determines the height of the nested list
;;   nln.
;; nheight: Nested-Listof-X -> Nat
;; Example
(check-expect (nheight '()) 1)
(check-expect (nheight '(a b c)) 1)

(define (nheight nln)
  (nfoldr (lambda (f r)
             r)
          (lambda (f r)
            (max (add1 f) r))
          1 nln))

;; Tests
(check-expect (nheight empty) 1)
(check-expect (nheight '(1 (2 3) () ((4)))) 3)
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)


;; ===================== (f) ===========================
;; (prune nln) consumes nln and removes all empty lists and
;;   any nested empty lists in nln
;; prune: Nested-Listof-X -> Nested-Listof-X
;; Example
(check-expect (prune empty) empty)
(check-expect (prune '(1 (2 3 ()) ( (()) (4) ()(()))))
              '(1 (2 3) ((4))))

(define (prune nln)
  (nfoldr (lambda (f r)
            (cons f r))
          (lambda (f r)
            (cond [(empty? f) r]
                  [else
                   (cons f r)]))
          empty nln))

;; Tests
(check-expect (prune empty) empty)
(check-expect (prune '(1 (2 3) () ((4)))) '(1 (2 3) ((4))))
(check-expect (prune '(()((())()))) empty)