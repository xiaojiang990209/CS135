;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ulam) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zijie Jiang (20714726)
;; CS135 Fall 2017
;; Assignment 05, Problem Bonus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "drawinglib.rkt")


;; (change-direction dir) changes dir to the next appropriate
;;    direction.
;; change-direction: (anyof 'L 'R 'U 'D) -> (anyof 'L 'R 'U 'D)
;; Example
(check-expect (change-direction 'U) 'L)

(define (change-direction dir)
  (cond [(symbol=? dir 'R) 'U]
        [(symbol=? dir 'U) 'L]
        [(symbol=? dir 'L) 'D]
        [(symbol=? dir 'D) 'R]
        [else
         (error "Invalid direction")]))


;; (compute-cur-x dir cur-x length) computes the next x-coordinate
;;    based on current dir, cur-x, and length of the square
;; compute-cur-x: (anyof 'L 'R 'U 'D) Nat Nat -> Nat
;; Example
(check-expect (compute-cur-x 'L 30 10) 20)

(define (compute-cur-x dir cur-x length)
  (cond [(symbol=? dir 'R) (+ cur-x length)]
        [(symbol=? dir 'L) (- cur-x length)]
        [else cur-x]))


;; (compute-cur-y dir cur-y length) computes the next y-coordinate
;;    based on current dir, cur-y, and length of the square
;; compute-cur-y: (anyof 'L 'R 'U 'D) Nat Nat -> Nat
;; Example
(check-expect (compute-cur-y 'U 30 10) 20)

(define (compute-cur-y dir cur-y length)
  (cond [(symbol=? dir 'U) (- cur-y length)]
        [(symbol=? dir 'D) (+ cur-y length)]
        [else cur-y]))


;; (prime-helper? start end) produces true if for every integer
;;   from start to end does not divide end, and false otherwise
;; prime-helper?: Nat Nat -> Bool
;; Example
(check-expect (prime-helper? 2 9) false)

(define (prime-helper? start end)
  (cond [(= end start) true]
        [(zero? end) false]
        [(= end 1) false]
        [(= 0 (remainder end start)) false]
        [else (prime-helper? (add1 start) end)]))


;; (prime? n) produces true if n is prime, and false otherwise
;; prime?: Nat -> Bool
;; Example
(check-expect (prime? 17) true)

(define (prime? n)
  (prime-helper? 2 n))


;; (ulam-list n end steps cur-x cur-y dir stepcounter add-step-counter length)
;;    produces a Drawing of ulam squares, starting from n to end, with a side
;;    length of length, cur-x, cur-y representing current coordinate, steps
;;    representing the number of steps have been moved since last changing
;;    direction, and stepcounter and add-step-counter determining whether we
;;    should increase the number of steps or change direction.
;; ulam-list: Nat Nat Nat Nat Nat (anyof 'L 'R 'U 'D)
;;            Nat (anyof 0 1) Nat -> Drawing

(define (ulam-list n end steps cur-x cur-y dir stepcounter add-step-counter length)
  (cond [(= n end) empty]
        [(prime? n)
         (cons (make-square135 (make-posn cur-x cur-y) length '(0 0 0))
               ;; Change direction, change # of steps
               (cond [(and (= stepcounter steps) (= add-step-counter 1))
                      (ulam-list (add1 n) end 1 
                            (compute-cur-x (change-direction dir) cur-x length)
                            (compute-cur-y (change-direction dir) cur-y length)
                            (change-direction dir) (add1 stepcounter) 0 length)]
                     ;; Change direction
                     [(= stepcounter steps)
                      (ulam-list (add1 n) end 1 
                            (compute-cur-x (change-direction dir) cur-x length)
                            (compute-cur-y (change-direction dir) cur-y length)
                            (change-direction dir) stepcounter
                            (add1 add-step-counter) length)]
                     ;; Does not change direction, does not change # of steps
                     [else (ulam-list (add1 n) end (add1 steps) 
                                 (compute-cur-x dir cur-x length)
                                 (compute-cur-y dir cur-y length)
                                 dir stepcounter add-step-counter length)]))]
        ;; Change direction, change # of steps
        [(and (= stepcounter steps) (= add-step-counter 1))
         (ulam-list (add1 n) end 1 
               (compute-cur-x (change-direction dir) cur-x length)
               (compute-cur-y (change-direction dir) cur-y length)
               (change-direction dir) (add1 stepcounter) 0 length)]
        ;; Change direction
        [(= stepcounter steps)
         (ulam-list (add1 n) end 1 
               (compute-cur-x (change-direction dir) cur-x length)
               (compute-cur-y (change-direction dir) cur-y length)
               (change-direction dir) stepcounter
               (add1 add-step-counter) length)]
        ;; Does not change direction, does not change # of steps
        [else (ulam-list (add1 n) end (add1 steps)
                    (compute-cur-x dir cur-x length)
                    (compute-cur-y dir cur-y length)
                    dir stepcounter add-step-counter
                    length)]))


;; (ulam-spiral num-row-column side-length) consumes num-row-column
;;   and side-length, and produces a Drawing containing a black square
;;   of the given side-length for each prime-numbered location.
;; ulam-spiral: Nat Nat -> Drawing

(define (ulam-spiral num-row-column side-length)
  (ulam-list 1 (add1 (sqr num-row-column)) 0 
        (- (/ (* num-row-column side-length) 2) 5)
        (- (/ (* num-row-column side-length) 2) 5)
        'R 1 0 side-length))