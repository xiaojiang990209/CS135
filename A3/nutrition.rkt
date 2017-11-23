;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *******************************************************
;;   Zijie Jiang (20714726)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 2
;; *******************************************************
;;

(define-struct nutri-fact (name serving fat carbs sugar protein))
;; A Nutri-fact is a (make-nutri-fact Str Num Num Num Num Num)
;; requires: 0 < serving
;;           fat + carbs + protein <= serving
;;           0 <= sugar <= carbs
;;           0 <= fat, protein


;; (a)
;; my-nutri-fact-fn: Nutri-Fact -> Any
(define (my-nutri-fact-fn nutri-fact)
  ( ... (nutri-fact-name nutri-fact) ... 
    ... (nutri-fact-serving nutri-fact) ...
    ... (nutri-fact-fat nutri-fact) ... 
    ... (nutri-fact-carbs nutri-fact) ... 
    ... (nutri-fact-sugar nutri-fact) ... 
    ... (nutri-fact-protein nutri-fact) ... ))


;; Useful converters
(define calories-per-gram-fat 9)
(define calories-per-gram-carbs 4)
(define calories-per-gram-protein 4)


;; Useful constants for examples and testing
(define honey-nut-cheerio
  (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
(define 58g-serving-honey-nut-cheerio
  (make-nutri-fact "Honey Nut Cheerios" 58 3 46 18 4))
(define milk (make-nutri-fact "Milk" 100 4 1 0 15))
(define 150g-serving-milk (make-nutri-fact "Milk" 150 6 1.5 0 22.5))
(define vegetable (make-nutri-fact "Vegetable" 50 0 5 0 2))
(define coke (make-nutri-fact "Coke" 100 20 30 25 1))
(define protein-powder (make-nutri-fact "Protein Powder" 30 5 1 0 25))
(define protein-powder-with-more-carbs
  (make-nutri-fact "Protein Powder More carbs" 30 5 10 0 25))
(define protein-powder-with-less-fat
  (make-nutri-fact "Protein Powder Less fat" 30 3 1 0 25))
(define invalid-name-type-nutri-fact
  (make-nutri-fact 'name 20 0 10 5 5))
(define invalid-serving-type-nutri-fact
  (make-nutri-fact "Invalid-serving-type" "Invalid" 0 10 5 5))
(define invalid-fat-type-nutri-fact
  (make-nutri-fact "Invalid-fat-type" 20 "Invalid" 10 5 5))
(define invalid-carbs-type-nutri-fact
  (make-nutri-fact "Invalid-carbs-type" 20 0 "Invalid" 5 5 ))
(define invalid-sugar-type-nutri-fact
  (make-nutri-fact "Invalid-sugar-type" 20 0 10 "Invalid" 5))
(define invalid-protein-type-nutri-fact
  (make-nutri-fact "Invalid-protein-type" 20 0 10 5 "Invalid"))
(define invalid-serving-amount-nutri-fact
  (make-nutri-fact "Invalid-serv-amount" -5 0 10 5 5))
(define invalid-fat-carbs-protein-amount-nutri-fact
  (make-nutri-fact "Invalid-carbs-protein-fat-amount" 20 10 5 0 10))
(define invalid-sugar-amount-nutri-fact
  (make-nutri-fact "Invalid-sugar-amount" 20 1 2 3 5))
(define invalid-protein-amount-nutri-fact
  (make-nutri-fact "Invalid-protein-amount" 20 1 3 2 -5))
(define invalid-fat-amount-nutri-fact
  (make-nutri-fact "Invalid-fat-amount" 20 -5 3 1 5))


;; *********************************************************************************
;; (b)
;; (calc-new-serving-portion nutri-fact-amount new-serving old-serving)
;;    calculates the proportional amount of nutrients according
;;    to the original nutri-fact-amount, new-serving size and
;;    old-serving size
;; calc-new-serving-portion: Num Num Num -> Num
;; requires: 0 <= nutri-fact-amount
;;           0 < new-serving, old-serving
;; Example
(check-expect (calc-new-serving-portion 1.5 58 29) 3)

(define (calc-new-serving-portion nutri-fact-amount
                                  new-serving old-serving)
  (* (/ new-serving old-serving) nutri-fact-amount))

;; (resize nutri-fact new-serving) produces a new Nutri-Fact with
;;    the new-serving size and all of the other numerical nutrient
;;    fields of the original nutri-fact changed to reflect the
;;    new serving size.
;; resize: Nutri-Fact Num -> Nutri-Fact
;; requires: new-serving > 0
;; Example:
(check-expect (resize honey-nut-cheerio 58)
              58g-serving-honey-nut-cheerio)

(define (resize nutri-fact new-serving)
  (make-nutri-fact
   (nutri-fact-name nutri-fact)
   new-serving
   (calc-new-serving-portion (nutri-fact-fat nutri-fact)
                             new-serving
                             (nutri-fact-serving nutri-fact))
   (calc-new-serving-portion (nutri-fact-carbs nutri-fact)
                             new-serving
                             (nutri-fact-serving nutri-fact))
   (calc-new-serving-portion (nutri-fact-sugar nutri-fact)
                             new-serving
                             (nutri-fact-serving nutri-fact))
   (calc-new-serving-portion (nutri-fact-protein nutri-fact)
                             new-serving
                             (nutri-fact-serving nutri-fact))))

;; Tests
(check-expect (resize milk 150) 150g-serving-milk)


;;**********************************************************************************
;;(c)
;; (calories nutri-fact) produces the number of calories there
;;    are in a serving of nutri-fact
;; calories: Nutri-Fact -> Num
;; Example:
(check-expect (calories honey-nut-cheerio) 113.5)

(define (calories nutri-fact)
  (+ (* (nutri-fact-fat nutri-fact) calories-per-gram-fat)
     (* (nutri-fact-carbs nutri-fact) calories-per-gram-carbs)
     (* (nutri-fact-protein nutri-fact) calories-per-gram-protein)))

;; Tests
(check-expect (calories milk) 100)


;;**********************************************************************************
;;(d)
;; (calc-proportion-nutrient nutri-fact nutrient-name) produces
;;    the quantity of nutrient-name as a proportion of the entire
;;    serving size of nutri-fact
;; calc-proportion-nutrient: Nutri-Fact Sym -> Num
;; Example
(check-expect (calc-proportion-nutrient milk 'protein) 0.15)

(define (calc-proportion-nutrient nutri-fact nutrient-name)
  (cond [(symbol=? nutrient-name 'sugar)
         (/ (nutri-fact-sugar nutri-fact)
            (nutri-fact-serving nutri-fact))]
        [(symbol=? nutrient-name 'protein)
         (/ (nutri-fact-protein nutri-fact)
            (nutri-fact-serving nutri-fact))]
        [(symbol=? nutrient-name 'carbs)
         (/ (nutri-fact-carbs nutri-fact)
            (nutri-fact-serving nutri-fact))]
        [(symbol=? nutrient-name 'fat)
         (/ (nutri-fact-fat nutri-fact)
            (nutri-fact-serving nutri-fact))]))

;; (choose-for-diet nutri-fact-1 nutri-fact-2) selects the one that
;;    is the most appropriate for a friend's diet, from nutri-fact-1
;;    and nutri-fact-2, according to specific rules, or nutri-fact-1,
;;    if they are identical.
;; choose-for-diet: Nutri-Fact Nutri-Fact -> Nutri-Fact
;; Example
(check-expect (choose-for-diet vegetable coke) vegetable)
(check-expect (choose-for-diet coke vegetable) vegetable)

(define (choose-for-diet nutri-fact-1 nutri-fact-2)
  (cond [(< (calc-proportion-nutrient nutri-fact-1 'sugar)
            (calc-proportion-nutrient nutri-fact-2 'sugar))
         nutri-fact-1]
        [(> (calc-proportion-nutrient nutri-fact-1 'sugar)
            (calc-proportion-nutrient nutri-fact-2 'sugar))
         nutri-fact-2]
        [(> (calc-proportion-nutrient nutri-fact-1 'protein)
            (calc-proportion-nutrient nutri-fact-2 'protein))
         nutri-fact-1]
        [(< (calc-proportion-nutrient nutri-fact-1 'protein)
            (calc-proportion-nutrient nutri-fact-2 'protein))
         nutri-fact-2]
        [(< (calc-proportion-nutrient nutri-fact-1 'carbs)
            (calc-proportion-nutrient nutri-fact-2 'carbs))
         nutri-fact-1]
        [(> (calc-proportion-nutrient nutri-fact-1 'carbs)
            (calc-proportion-nutrient nutri-fact-2 'carbs))
         nutri-fact-2]
        [(< (calc-proportion-nutrient nutri-fact-1 'fat)
            (calc-proportion-nutrient nutri-fact-2 'fat))
         nutri-fact-1]
        [(> (calc-proportion-nutrient nutri-fact-1 'fat)
            (calc-proportion-nutrient nutri-fact-2 'fat))
         nutri-fact-2]
        [else nutri-fact-1]))

;; Tests
(check-expect (choose-for-diet milk 150g-serving-milk) milk)
(check-expect (choose-for-diet protein-powder vegetable)
              protein-powder)
(check-expect (choose-for-diet vegetable protein-powder)
              protein-powder)
(check-expect (choose-for-diet protein-powder
                               protein-powder-with-more-carbs)
              protein-powder)
(check-expect (choose-for-diet protein-powder-with-more-carbs
                               protein-powder)
              protein-powder)
(check-expect (choose-for-diet protein-powder
                               protein-powder-with-less-fat)
              protein-powder-with-less-fat)
(check-expect (choose-for-diet protein-powder-with-less-fat
                               protein-powder)
              protein-powder-with-less-fat)


;;**********************************************************************************
;;(e)
;; (valid-nutri-fact? nutri-fact) consumes an arbitary value(Any)
;;    and produces true if it is a valid Nutri-Fact, and false
;;    otherwise
;; valid-nutri-fact?: Any -> Bool
;; Example
(check-expect (valid-nutri-fact? "Im not a Nutri-fact") false)
(check-expect (valid-nutri-fact? milk) true)

(define (valid-nutri-fact? nutri-fact)
  (and (nutri-fact? nutri-fact)
       (string? (nutri-fact-name nutri-fact))
       (number? (nutri-fact-serving nutri-fact))
       (number? (nutri-fact-fat nutri-fact))
       (number? (nutri-fact-carbs nutri-fact))
       (number? (nutri-fact-sugar nutri-fact))
       (number? (nutri-fact-protein nutri-fact))
       (> (nutri-fact-serving nutri-fact) 0)
       (<= (+ (nutri-fact-fat nutri-fact)
             (nutri-fact-carbs nutri-fact)
             (nutri-fact-protein nutri-fact))
          (nutri-fact-serving nutri-fact))
       (and (<= 0 (nutri-fact-sugar nutri-fact))
           (<= (nutri-fact-sugar nutri-fact)
               (nutri-fact-carbs nutri-fact)))
       (and (<= 0 (nutri-fact-fat nutri-fact))
           (<= 0 (nutri-fact-protein nutri-fact)))))

;; Tests
(check-expect (valid-nutri-fact? invalid-name-type-nutri-fact) false)
(check-expect (valid-nutri-fact? invalid-serving-type-nutri-fact) false)
(check-expect (valid-nutri-fact? invalid-fat-type-nutri-fact) false)
(check-expect (valid-nutri-fact? invalid-carbs-type-nutri-fact) false)
(check-expect (valid-nutri-fact? invalid-sugar-type-nutri-fact) false)
(check-expect (valid-nutri-fact? invalid-protein-type-nutri-fact) false)
(check-expect (valid-nutri-fact?
               invalid-serving-amount-nutri-fact) false)
(check-expect (valid-nutri-fact?
               invalid-fat-carbs-protein-amount-nutri-fact) false)
(check-expect (valid-nutri-fact?
               invalid-sugar-amount-nutri-fact) false)
(check-expect (valid-nutri-fact?
               invalid-protein-amount-nutri-fact) false)
(check-expect (valid-nutri-fact?
               invalid-fat-amount-nutri-fact) false)