;; exercise 1.1
10                                      ;10
(+ 5 3 4)                               ;12
(- 9 1)                                 ;8
(/ 6 2)                                 ;3
(+ (* 2 4) (- 4 6))                     ;6
(define a 3)                            ;[3]
(define b (+ a 1))                      ;[4]
(+ a b (* a b))                         ;19
(= a b)                                 ;#f
(if (and (> b a) (< b (* a b)))
    b
    a)                                  ;4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                        ;16

(+ 2 (if (> b a) b a))                  ;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                             ;16


;; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))               ;-37/150

;; (5 + 4 + (2 - (3 - (6 + 4 / 5)))) = 14.8 = 74/5
;; 3 * (6 - 2) * (2 - 7) = -60
;; ((74 / 5) / (-60)) * 150 = -37.0000000001


;; exercise 1.3
(define (max a b)
  (if (> a b)
      a
      b))
(max 10 10)
(define (min a b)
  (if (< a b)
      a
      b))
(min 5 3)
(define (square a)
  (* a a))
(define (sum-of-squares a b)
  (+ (square a) (square b)))
(define (sum-of-squares-largest-two a b c)
  (+ (square (max a b)) (square (max (min a b) c))))
(sum-of-squares-largest-two 1 2 3)      ;13
(sum-of-squares-largest-two 3 2 1)      ;13
(sum-of-squares-largest-two 2 3 1)      ;13

;; exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -4)
(a-plus-abs-b 3 4)

;; exercise 1.5
(define (p) (p))
;(p) ;; infinite loop
(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p)) ;; infinite loop -> Guile seems to use applicative order
              ;; -> (p) is evaluated before the if is encountered
