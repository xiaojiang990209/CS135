;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (quick-sort lon)
  (cond [(empty? lon) empty]
        [else
         (local [(define pivot (first lon))
                 (define left-subproblem (filter (lambda (x) (< x pivot)) lon))
                 (define right-subproblem (filter (lambda (x) (>= x pivot)) (rest lon)))
                 ]
           (append (quick-sort left-subproblem)
                   (list pivot)
                   (quick-sort right-subproblem)))]))


(define (double-list lst)
  (map (lambda (f)
         (* 2 f))
       lst))
(double-list '(1 2 3 4 5))

(define (div-by-3 lst)
  (filter (lambda (x)
            (= (remainder x 3) 0))
          lst))

(div-by-3 '(1 2 3 4 5 6 7))

(filter odd? (build-list 12 add1))

(define (factors n)
  (filter (lambda (x) (zero? (remainder n x))) (build-list n add1)))


(foldr (lambda (f r)
         (cond [(< f r) f]
               [else r]))
       (first '(6 1 2 9 2)) '(6 1 2 9 2))

(foldr (lambda (f r)
         (cond [(even? f) (add1 r)]
               [else r]))
       0 '(1 2 3 4 5 6 7))

'(2 8)

(define (separate lst elem)
  (foldr (lambda (f r)
           (cond [(empty? r) (list f)]
                 [(equal? f (rest r))
                  (cons f (cons elem r))]
                 [else
                  (cons f r)]))
         empty lst))

(define (list->lines loc)
  (cond [(empty? loc) empty]
        [else
         (local [(define r (list->lines (rest loc)))]
           (cond [(char=? (first loc) #\newline)
                  (cons empty r)]
                 [(empty? r)
                  (list (list (first loc)))]
                 [else
                  (cons (cons (first loc) (first r))
                        (rest r))]))]))

(define (string->strlines str)
  (map list->string
       (list->lines (string->list str))))


(define (first-line loc)
  (cond [(empty? loc) empty]
        [(char=? (first loc) #\newline) empty]
        [else
         (cons (first loc) (first-line (rest loc)))]))

(define (rest-of-lines loc)
  (cond [(empty? (rest loc)) empty]
        [(char=? (first loc) #\newline)
         (rest loc)]
        [else
         (rest-of-lines (rest loc))]))


;; foldl: (X Y -> Y) Y (listof X) -> Y
(define (my-foldl combine base lst)
  (local [(define (foldl/acc lst acc)
            (cond [(empty? lst) acc]
                  [else
                   (foldl/acc (rest lst)
                              (combine (first lst) acc))]))]
    (foldl/acc lst base)))

;; (foldr f b (list x1 x2 ... xn))
;;  => (f x1 (f x2 (... (f xn b))))

;; (foldl f b (list x1 x2 ... xn))
;;  => (f xn (f x_n-1 (... (f x1 b))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define G '((A (C D E))
            (B (E J))
            (C ())
            (D (F J))
            (E (K))
            (F (K H))
            (H ())
            (J (H))
            (K ())))

;; (neighbours graph node) produces the out-neighbours
;; of node in the graph
;; neighbouts: Graph Node -> (listof Node)
;; requires: node exist in graph
(check-expect (neighbours G 'D) '(F J))
(define (neighbours graph node)
  (cond [(empty? graph) (error "node not in graph")]
        [(symbol=? (first (first G)) node)
         (second (first G))]
        [else
         (neighbours (rest graph) node)]))

;;(define (neighbours graph node)
;;  (second (first (filter (lambda (pair) (symbol=? node (first pair)))
;;          graph))))

(check-expect (in-neighbours G 'J) '(B D))
(define (in-neighbours graph node)
  (map first (filter (lambda (pair)
            (member? node (second pair)))
          graph)))

;; (find-route orig dest G) finds route from orig to dest in G if it exists
;; find-route: Node Node Graph -> (anyof (list Node) false)
(define (find-route orig dest G)
  (cond [(symbol=? orig dest) (list orig)]
        [else (local [(define nbrs (neighbours orig G))
                      (define route (find-route/list nbrs dest G))]
                (cond [(false? route) route]
                      [else
                       (cons orig route)]))]))

(define (find-route/list los dest G)
  (cond [(empty? los) false]
        [else
         (local [(define route (find-route (first los) dest G))]
           (cond [(false? route)
                  (find-route/list (rest los) dest G)]
                 [else route]))]))