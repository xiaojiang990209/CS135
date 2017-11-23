;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname trie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS135 Fall 2017
;; Zijie Jiang (20714726)
;; Assignment 7 Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "a07lib.rkt")


;; Useful constants for examples and testing
(define h-u-trie-words
  (list "ha" "hat" "he" "hot" "use"))
(define a-c-trie-words
  (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))
(define c-d-trie-words
  (list "cat" "catch" "cater" "catnip" "cattle"
        "dig" "dog" "dogfish" "donald" "donut"
        "doze"))
(define empty-trie
  (make-trie empty))
(define h-u-trie-with-hated
  (make-trie
   (list
    (make-tnode
     #\h
     false
     (list
      (make-tnode
       #\a true
       (list (make-tnode
              #\t true
              (list (make-tnode
                     #\e false
                     (list (make-tnode #\d true empty)))))))
      (make-tnode #\e true empty)
      (make-tnode #\o false (list (make-tnode #\t true empty)))))
    (make-tnode
     #\u false
     (list (make-tnode #\s false (list (make-tnode #\e true empty))))))))
(define h-u-trie-with-words-inserted
  (make-trie
   (list
    (make-tnode
     #\h
     false
     (list
      (make-tnode
       #\a true
       (list (make-tnode
              #\t true
              (list (make-tnode
                     #\e false
                     (list (make-tnode #\d true empty)))))))
      (make-tnode #\e true empty)
      (make-tnode #\i false (list (make-tnode #\m true empty)))
      (make-tnode #\o true (list (make-tnode #\t true empty)))))
    (make-tnode
     #\u false
     (list (make-tnode #\s false (list (make-tnode #\e true empty)))))
    (make-tnode
     #\w false
     (list
      (make-tnode
       #\i false
       (list
        (make-tnode #\t false (list (make-tnode #\h true empty))))))))))
(define def-lst-tnode
  (list
   (make-tnode #\d true empty)
   (make-tnode #\e true empty)
   (make-tnode #\f true empty)))
  

;; =========(A)===========
(define a-tnode
  (make-tnode #\a false
              (list (make-tnode #\t true empty))))
(define c-tnode
  (make-tnode
   #\c
   false
   (list
    (make-tnode
     #\o
     false
     (list
      (make-tnode #\o true empty)
      (make-tnode #\w true empty)))
    (make-tnode
     #\s
     false
     (list
      (make-tnode
       #\1
       false
       (list
        (make-tnode
         #\1
         false
         (list
          (make-tnode #\5 true empty)
          (make-tnode #\6 true empty)))
        (make-tnode
         #\3
         false
         (list
          (make-tnode #\5 true empty)
          (make-tnode #\6 true empty))))))))))
(define a-c-trie
  (make-trie (list a-tnode c-tnode)))


;; =========(B)===========
;; trie-template: Trie -> Any
;;(define (trie-template trie)
;;  (cond [(empty? trie) ...]
;;        [else
;;         ( ... (list-tnode-template (trie-children)) ... )]))


;; list-tnode-template: (listof TNode) -> Any
;;(define (list-tnode-template lst)
;;  (cond [(empty? lst) ...]
;;        [else
;;         (... (tnode-template (first lst)) ...
;;              (list-tnode-template (rest lst)) ... )]))


;; tnode-template: TNode -> Any
;;(define (tnode-template tnode)
;;  ( ... (tnode-key tnode) ...
;;        (tnode-ends-word? tnode) ...
;;        (list-tnode-template (tnode-children tnode)) ...))


;; =========(C)===========
;; (in-list-tnode? chars lst) produces true if chars is present in
;;   lst, and false otherwise
;; in-list-tnode?: (listof Char) (listof TNode) -> Bool
;; Example
(check-expect (in-list-tnode? (list #\a) empty) false)
(check-expect (in-list-tnode? (list #\d) def-lst-tnode) true)

(define (in-list-tnode? chars lst)
  (cond [(empty? lst) false]
        [else
         (or (in-tnode? chars (first lst))
             (in-list-tnode? chars (rest lst)))]))


;; (in-tnode? chars tnode) produces true if chars is present in tnode,
;;    and false otherwise
;; in-tnode?: (listof Char) TNode -> Bool
;; Example
(check-expect (in-tnode? (list #\a) (make-tnode #\a true empty)) true)

(define (in-tnode? chars tnode)
  (cond [(and (empty? (rest chars))
              (tnode-ends-word? tnode)
              (char=? (first chars) (tnode-key tnode)))
         true]
        [(empty? (rest chars)) false]
        [(char=? (first chars) (tnode-key tnode))
         (in-list-tnode? (rest chars) (tnode-children tnode))]
        [else false]))
  

;; (in-trie? word trie) produces true if word is in the trie,
;;   and false otherwise.
;; in-trie?: Str Trie -> Bool
;; Example
(check-expect (in-trie? "cs" a-c-trie) false)
(check-expect (in-trie? "cs115" a-c-trie) true)
(check-expect (in-trie? "cower" a-c-trie) false)
(check-expect (in-trie? "cs115" empty-trie) false)

(define (in-trie? word trie)
  (in-list-tnode? (string->list word) (trie-children trie)))

;; Tests
(check-expect (in-trie? "cs115" empty-trie) false)
(check-expect (in-trie? "hat" h-u-trie) true)


;; ==============(D)==================
;; (list-words-tnode/acc tnode chars) takes a tnode and currently
;;   accumulated chars, and produce a list of words under tnode
;; list-words-tnode/acc: TNode (listof Char) -> (listof Str)
;; Example
(check-expect (list-words-tnode/acc a-tnode empty) (list "at"))

(define (list-words-tnode/acc tnode chars)
  (cond [(tnode-ends-word? tnode)
         (append (list (list->string
                        (reverse (cons (tnode-key tnode) chars))))
                 (list-words-lst-tnode/acc (tnode-children tnode)
                                           (cons (tnode-key tnode) chars)))]
        [else
         (list-words-lst-tnode/acc (tnode-children tnode)
                                   (cons (tnode-key tnode) chars))]))


;; (list-words-lst-tnode/acc lstnode chars) takes a list of TNode,
;;    lstnode, and the currently accumulated chars, and produces
;;    a list of string in lstnode
;; list-words-lst-tnode/acc: (listof TNode) (listof Char) -> (listof Str)
;; Example
(check-expect (list-words-lst-tnode/acc (list a-tnode c-tnode) empty)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))
(check-expect (list-words-lst-tnode/acc empty empty) empty)

(define (list-words-lst-tnode/acc lstnode chars)
  (cond [(empty? lstnode) empty]
        [else
         (append (list-words-tnode/acc (first lstnode) chars)
                 (list-words-lst-tnode/acc (rest lstnode) chars))]))


;; (list-words trie) consumes trie and produces a list of all
;;    the words in trie.
;; list-words: Trie -> (listof Str)
;; Examples:
(check-expect (list-words h-u-trie) h-u-trie-words)
(check-expect (list-words a-c-trie) a-c-trie-words)
(check-expect (list-words empty-trie) empty)

(define (list-words trie)
  (list-words-lst-tnode/acc (trie-children trie) empty))

;; Tests
(check-expect (list-words c-d-trie) c-d-trie-words)
(check-expect (list-words empty-trie) empty)


;; ==============(E)==================
;; (create-tnode-from-chars chars) consumes chars and produces a nested
;;    TNode containing these chars, with the last character of
;;    the list to have a true ends-word value in the assoicated
;;    tnode
;; create-tnode-from-chars: (listof Char) -> TNode
;; Example
(check-expect (create-tnode-from-chars (list #\a #\b))
              (make-tnode #\a false (list (make-tnode #\b true empty))))

(define (create-tnode-from-chars chars)
  (cond [(empty? (rest chars))
         (make-tnode (first chars) true empty)]
        [else
         (make-tnode (first chars) false
                     (list (create-tnode-from-chars (rest chars))))]))


;; (insert-word-tnode chars tnode) consumes chars and tnode, and
;;    produces the tnode with chars inserted, if chars can be
;;    inserted into tnode
;; insert-word-tnode: (listof Char) TNode -> TNode
;; requires: chars is not empty
;;           (first chars) is equal to (tnode-key tnode)
;; Example
(check-expect (insert-word-tnode (list #\a #\c) a-tnode)
              (make-tnode #\a false (list (make-tnode #\c true empty)
                                          (make-tnode #\t true empty))))

(define (insert-word-tnode chars tnode)
  (cond [(empty? (tnode-children tnode))
         (make-tnode (tnode-key tnode)
                     (tnode-ends-word? tnode)
                     (list (create-tnode-from-chars (rest chars))))]
        [(empty? (rest chars))
         (make-tnode (tnode-key tnode)
                     true
                     (tnode-children tnode))]
        [else
         (make-tnode (tnode-key tnode)
                     (tnode-ends-word? tnode)
                     (insert-word-lst-tnode (rest chars)
                                            (tnode-children tnode)))]))

 
;; (insert-word-lst-tnode chars lst) consumes chars and lst, a list of
;;    TNodes, and produces a list of TNodes with the word inserted.
;; insert-word-lst-tnode: (listof Char) (listof TNode) -> (listof TNode)
;; requires: chars is non-empty
;; Example
(check-expect (insert-word-lst-tnode (list #\c #\a #\t)
                                     (list a-tnode))
              (list a-tnode
                    (make-tnode
                     #\c false
                     (list (make-tnode
                            #\a false
                            (list (make-tnode #\t true empty)))))))
(check-expect (insert-word-lst-tnode (list #\c #\a #\t) empty)
              (list (make-tnode
                     #\c false
                     (list (make-tnode
                            #\a false
                            (list (make-tnode #\t true empty)))))))

(define (insert-word-lst-tnode chars lst)
  (cond [(empty? lst) (list (create-tnode-from-chars chars))]
        [(char=? (tnode-key (first lst)) (first chars))
         (cons (insert-word-tnode chars (first lst)) (rest lst))]
        [(char<? (first chars) (tnode-key (first lst)))
         (cons (create-tnode-from-chars chars) lst)]
        [(empty? (rest lst))
         (cons (first lst)
               (cons (create-tnode-from-chars chars) empty))]
        [else
         (cons (first lst) (insert-word-lst-tnode chars (rest lst)))]))


;; (insert-word word trie) consumes trie and word, and produces a
;;    Trie with the word inserted.
;; insert-word: Str Trie -> Trie
;; Examples
(check-expect (insert-word "at" empty-trie)
              (make-trie (list a-tnode)))
(check-expect (insert-word "hated" h-u-trie)
              h-u-trie-with-hated)

(define (insert-word word trie)
   (cond [(in-trie? word trie) trie]
         [else (make-trie
                (insert-word-lst-tnode (string->list word)
                                       (trie-children trie)))]))

;; Tests
(check-expect (insert-word "he" h-u-trie) h-u-trie)
(check-expect (insert-word "at" empty-trie)
              (make-trie (list a-tnode)))
(check-expect (list-words (insert-word "ho" h-u-trie))
              (list "ha" "hat" "he" "ho" "hot" "use"))
(check-expect (list-words (insert-word "him" h-u-trie))
              (list "ha" "hat" "he" "him" "hot" "use"))
(check-expect (list-words (insert-word "with" h-u-trie))
              (list "ha" "hat" "he" "hot" "use" "with"))


;; ================ (f) ====================
;; (insert-some-words words trie) consumes words, a list of string
;;   and trie, and produces a Trie consisting of the consumed
;;   Trie with all of the strings inserted.
;; insert-some-words: (listof Str) Trie -> Trie
;; Example
(check-expect (list-words (insert-some-words
                           (list "with" "ho" "him") h-u-trie))
              (list "ha" "hat" "he" "him" "ho" "hot" "use" "with"))
(check-expect (list-words (insert-some-words
                           (list "with" "ho" "him") empty-trie))
              (list "him" "ho" "with"))

(define (insert-some-words words trie)
  (local
    [(define (insert-some-words/acc words trie-so-far)
       (cond [(empty? words) trie-so-far]
             [else
              (insert-some-words/acc (rest words)
                                     (insert-word (first words)
                                                  trie-so-far))]))]
    (insert-some-words/acc words trie)))

;; Tests
(check-expect (list-words (insert-some-words
                           (list "with" "ho" "him") empty-trie))
              (list "him" "ho" "with"))
(check-expect (insert-some-words
               (list "with" "ho" "him" "hated") h-u-trie)
              h-u-trie-with-words-inserted)

;; ================ (g) ====================
;; (list-completion-tnode/acc prefix tnode chars) consumes prefix and tnode
;;   and currently accumulated chars, and produce a list of string
;;   corresponding to that prefix.
;; list-completion-tnode: (listof Char) TNode (listof Char)
;;                        -> (listof Str)
;; Example
(check-expect (list-completions-tnode/acc empty a-tnode empty)
              (list "at"))
(check-expect (list-completions-tnode/acc (list #\c #\s) c-tnode empty)
              (list "cs115" "cs116" "cs135" "cs136"))

(define (list-completions-tnode/acc prefix tnode chars)
  (cond [(and (empty? prefix)
              (tnode-ends-word? tnode))
         (append (list (list->string
                        (reverse (cons (tnode-key tnode) chars))))
                 (list-completions-lst-tnode/acc
                  prefix (tnode-children tnode)
                  (cons (tnode-key tnode) chars)))]
        [(empty? prefix)
         (list-completions-lst-tnode/acc
          prefix (tnode-children tnode)
          (cons (tnode-key tnode) chars))]
        [(and (char=? (tnode-key tnode) (first prefix))
              (tnode-ends-word? tnode))
         (append (list (list->string
                        (reverse (cons (tnode-key tnode) chars))))
                 (list-completions-lst-tnode/acc
                  (rest prefix) (tnode-children tnode)
                  (cons (tnode-key tnode) chars)))]
        [(char=? (tnode-key tnode) (first prefix))
         (list-completions-lst-tnode/acc
          (rest prefix) (tnode-children tnode)
          (cons (tnode-key tnode) chars))]
        [else empty]))
         

;; (list-completion-lst-tnode/acc prefix lstnode chars) consumes prefix and
;;   lstnode and currently accumulated chars, and produce a list of string
;;   corresponding to that prefix in the trie.
;; list-completion-lst-tnode: (listof Char) (listof TNode) (listof Char)
;;                            -> (listof Str)
;; Example
(check-expect (list-completions-lst-tnode/acc
               empty (list a-tnode c-tnode) empty)
              (list "at" "coo" "cow" "cs115" "cs116"
                    "cs135" "cs136"))
(check-expect (list-completions-lst-tnode/acc
               (list #\c) empty empty) empty)

(define (list-completions-lst-tnode/acc prefix lstnode chars)
  (cond [(empty? lstnode) empty]
        [else
         (append (list-completions-tnode/acc
                  prefix (first lstnode) chars)
                 (list-completions-lst-tnode/acc
                  prefix (rest lstnode) chars))]))


;; (list-completions prefix trie) consumes prefix and trie, and
;;   produces a sorted list of strings consisting of all words
;;   in trie that begin with prefix
;; list-completions: Str Trie -> (listof Str)
;; Example
(check-expect (list-completions "don" c-d-trie)
              (list "donald" "donut"))
(check-expect (list-completions "" h-u-trie)
              (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-completions "ha" empty-trie)
              empty)

(define (list-completions prefix trie)
  (list-completions-lst-tnode/acc
   (string->list prefix) (trie-children trie) empty))

;; Tests
(check-expect (list-completions "" h-u-trie)
              (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-completions "ha" empty-trie)
              empty)
(check-expect (list-completions "ha" h-u-trie)
              (list "ha" "hat"))