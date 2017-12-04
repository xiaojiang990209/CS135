;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname turing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (forever x)
  (forever x))

(define (halts? f x)
  ;; do something teribly clever
  )

(define (T f)
  (cond [(halts? f f) (forever 1)]
        [else 1]))

(T T)