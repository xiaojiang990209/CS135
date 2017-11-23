;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 7, Bonus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a07lib.rkt")

(define (create-tnode-from-chars chars)
  (cond [(empty? (rest chars))
         (make-tnode (first chars) true empty)]
        [else
         (make-tnode (first chars) false
                     (list (create-tnode-from-chars (rest chars))))]))


(define (remove-word-tnode chars tnode)
  ;; When in here, it is certain that (first chars) = (tnode-key tnode)
  (cond [(and (empty? (rest chars)) (empty? (tnode-children tnode)))
         empty]
        [(and (empty? (rest chars)) (not (empty? (tnode-children tnode))))
         (make-tnode (tnode-key tnode)
                     false (tnode-children tnode))]
        [(and (cons? chars) (tnode-ends-word? tnode) (not (empty? (tnode-children tnode))))
         (cond [(equal? (create-tnode-from-chars chars)
                        (make-tnode (tnode-key tnode)
                                    false (tnode-children tnode)))
                (make-tnode (tnode-key tnode)
                            (tnode-ends-word? tnode)
                            empty)]
               [else
                (make-tnode (tnode-key tnode)
                            (tnode-ends-word? tnode)
                            (remove-word-lst-tnode (rest chars) (tnode-children tnode)))])]
        [(and (cons? chars) (not (tnode-ends-word? tnode)))
         (cond [(equal? (create-tnode-from-chars chars) tnode) empty]
               [else (make-tnode (tnode-key tnode)
                                 (tnode-ends-word? tnode)
                                 (remove-word-lst-tnode (rest chars) (tnode-children tnode)))])]
        [else
          (make-tnode (tnode-key tnode)
                      (tnode-ends-word? tnode)
                      (remove-word-lst-tnode (rest chars) (tnode-children tnode)))]))
        

;; Produce a list of TNodes
(define (remove-word-lst-tnode chars lst)
  (cond [(empty? lst) empty]
        ;; If (first lst) is the right one, run remove-tnode
        ;; on (first lst) while keeping the rest the same
        [(char=? (first chars) (tnode-key (first lst)))
         (local [(define result (remove-word-tnode chars (first lst)))]
           ;; If the returned tnode is empty, i.e. sentinel value
           ;; Produce rest lst directly
           (cond [(empty? result) (rest lst)]
           ;; Otherwise, produce (cons result (rest lst))
                 [else
                  (cons result (rest lst))]))]
        ;; Else, we look into the next one and keep the first one
        ;; unchanged.
        [else
         (cons (first lst) (remove-word-lst-tnode chars (rest lst)))]))

(define (remove-word str trie)
  (make-trie (remove-word-lst-tnode (string->list str)
                                    (trie-children trie))))
