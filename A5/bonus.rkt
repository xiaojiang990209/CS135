;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; If stepcounter == steps, which means walked the current required steps
;; need to change direction
;; If tcounter == 1 (0|1)
(define (draw n end steps dir curx cury stepcounter add-step-counter length)
  (cond [(= n end) empty]
        [(prime? n) (cons (make-square135 curx cury length '(0 0 0))
                            ;; Change direction, change # of steps
                            (cond [(and (= stepcounter steps) (= add-step-counter 1))
                                   (draw (add1 n) end 1 (change-direction dir)
                                         (compute-curx (change-direction dir) curx length)
                                         (compute-cury (change-direction dir) cury length)
                                         (add1 stepcounter) 0 length)]
                                  ;; Change direction
                                  [(= stepcounter steps)
                                   (draw (add1 n) end 1 (change-direction dir)
                                         (compute-curx (change-direction dir) curx length)
                                         (compute-cury (change-direction dir) cury length)
                                         stepcounter (add1 add-step-counter) length)]
                                  ;; Does not change direction, does not change # of steps
                                  [else (draw (add1 n) end (add1 steps) dir
                                              (compute-curx dir curx length)
                                              (compute-cury dir cury length)
                                              stepcounter tcounter length)]))]
        ;; Change direction, change # of steps
        [(and (= stepcounter steps) (= add-step-counter 1))
         (draw (add1 n) end 1 (change-direction dir)
               (compute-curx (change-direction dir) curx length)
               (compute-cury (change-direction dir) cury length)
               (add1 stepcounter) 0 length)]
        ;; Change direction
        [(= stepcounter steps)
         (draw (add1 n) end 1 (change-direction dir)
               (compute-curx (change-direction dir) curx length)
               (compute-cury (change-direction dir) cury length)
               stepcounter (add1 add-step-counter) length)]
        ;; Does not change direction, does not change # of steps
        [else (draw (add1 n) end (add1 steps) dir
                    (compute-curx dir curx length)
                    (compute-cury dir cury length)
                    stepcounter tcounter length)]))


(define (change-direction dir)
  (cond [(symbol=? dir 'R) 'U]
        [(symbol=? dir 'U) 'L]
        [(symbol=? dir 'L) 'D]
        [(symbol=? dir 'D) 'R]
        [else
         (error "Invalid direction")]))

(define (compute-curx dir curx length)
  (cond [(symbol=? dir 'R) (+ curx length)]
        [(symbol=? dir 'L) (- curx length)]
        [else curx]))

(draw 1 (add1 9) 0 'R 45 45 1 0 10)

(define (compute-cury dir cury length)
  (cond [(symbol=? dir 'U) (- cury length)]
        [(symbol=? dir 'D) (+ cury length)]
        [else cury]))

(define (prime? n)
  (prime-helper? 0 n))

(define (prime-helper? start end)
  (cond [(= end start) true]
        [(= 0 (remainder end start)) false]
        [else (prime-helper? (add1 start) end)]))