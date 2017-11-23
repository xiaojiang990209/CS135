;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An AL is one of
;; empty
;; (cons (list Any Nat) AL)


(define (make-AL-elem elem n)
  (list elem n))

(define (add-to-AL elem AL)
  (cond
;;  If AL is empty || elem is not in AL
    [(empty? AL) (cons (make-AL-elem elem 1) AL)]
;;  Compare elem to (first (first AL)) -> First element of AL, then take the first element of that element,
;;  which is of type Any
    [(equal? (first (first AL)) elem)
;;  then we have found another instance of elem; we add 1 to the Nat in (list Any Nat)
     (cons (make-AL-elem (first (first AL))
                         (add1 (second (first AL))))
           (rest AL))]
;;  if we have not found, then we proceed to the next element of AL
    [else
     (cons (first AL) (add-to-AL elem (rest AL)))]))

;;(make-AL-elem '(7 9)

(define (freq-count/acc list AL)
  (cond [(empty? list) AL]
        [else
         (freq-count/acc (rest list) (add-to-AL (first list) AL))]))

;;(define test_list '(red 7 9 (7 9) red 9))
(define test_list '(red 9 red))
(freq-count/acc test_list empty)