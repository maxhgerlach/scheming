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


;; Newton square root approx
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt (+ 100 37))
(square (sqrt 1000))


;; exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))
;(new-sqrt 9)
;;stack overflow: when using new-if instead of i, due to applicative
;;order new-sqrt-iter is called again and again although unnecessary
;;(all arguments to new-if are evaluated before the combination is
;;evaluated)


;; exercise 1.8
(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))
(define (square a)
  (* a a))
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 
     3))
(define (cbrt-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))
(define (cbrt x)
  (cbrt-iter 1.0 x))
(cbrt 27)


;; sqrt with internal definitions
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0))
(sqrt 15)
