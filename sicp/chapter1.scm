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


;; Chap. 1.2
(define (factorial n)
  (if (> n 1)
      (* n (factorial (- n 1)))
      1))
(factorial 6)

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))
(factorial 6)


;;Exercise 1.9
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
;; I: a linear recursive procedure (growing chain of inc's)
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(+ 4 5)
(if (= 4 0) 5 (inc (+ (dec 4) 5)))
(if #f 5 (inc (+ 3 5)))
(if #f 5 (inc (if (= 3 0) 5 (inc (+ (dec 3) 5)))))
(if #f 5 (inc (if #f 5 (inc (+ 2 5)))))
(if #f 5 (inc (if #f 5 (inc (if (= 2 0) 5 (inc (+ (dec 2) 5)))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (+ 1 5)))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if (= 1 0) 5 (inc (+ (dec 1) 5))) ))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (+ 0 5)))))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #t 5 (inc (+ (dec 0) 5)))))))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc 5))))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 6)))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 (inc 6))))))
(if #f 5 (inc (if #f 5 (inc (if #f 5 7)))))
(if #f 5 (inc (if #f 5 (inc 7))))
(if #f 5 (inc (if #f 5 8)))
(if #f 5 (inc 8))
(if #f 5 9)
9
;; actually the (if #f ..) would be simplified earlier

;; II: iterative -- discarding the if's the number of variables to hold state does not grow indefinitely
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

(+ 4 5)
(if (= 4 0) 5 (+ (dec 4) (inc 5)))
(if #f 5 (+ (dec 4) (inc 5)))
(if #f 5 (+ 3 6))
(if #f 5 (if (= 3 0) 6 (+ (dec 3) (inc 6))))
(if #f 5 (if #f 6 (+ 2 7)))
(if #f 5 (if #f 6 (if (= 2 0) 7 (+ (dec 2) (inc 7)))))
(if #f 5 (if #f 6 (if #f 7 (+ 1 8))))
(if #f 5 (if #f 6 (if (= a 1) 8 (+ (dec 1) (inc 8)))))
(if #f 5 (if #f 6 (if #f 8 (+ 0 9))))
(if #f 5 (if #f 6 (if #f 8 (if (= 0 0) 9 (+ (dec 0) (inc 9))))))
(if #f 5 (if #f 6 (if #f 8 (if #t 9 (+ -1 10)))))
(if #f 5 (if #f 6 (if #f 8 9)))
9


;; Exercise 1.10: Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(A 1 10)                                ;1024
(A 2 4)                                 ;65536
(A 3 3)                                 ;65536


(define (f n) (A 0 n))                  ; = 2*n

(define (g n) (A 1 n))                  ; (A 1 n) = (A 0 (A 1 (- n 1))) = (* 2 (A 1 (- n 1)))
                                        ; = 2**n for n > 0, 0 for n = 0

(define (h n) (A 2 n))                  ; (A 2 n) = (A 1 (A 2 (- n 1))) = (A 0 (A 1 (- (A 2 (- n 1)) 1)))
                                        ; = (* 2 (A 1 (- (A 2 (- n 1)) 1)))
                                        ; = 2 ** 2 ** 2 ** 2 ... [ with (n-1) powers ** ] 


;; Exercise 1.11: Fibonacci-like sequence
(define (fib-like n)
  (cond ((< n 3) n)
        (else (+ (fib-like (- n 1))
                 (* 2 (fib-like (- n 2)))
                 (* 3 (fib-like (- n 3)))))))

(define (fib-like-i n)
  (define (fib-like-iter a b c count)
    (if (> count n)
        a
        (fib-like-iter (+ a (* 2 b) (* 3 c))
                       a
                       b
                       (+ count 1))))
  (if (< n 3)
      n
      (fib-like-iter 2 1 0 3)))


;; Exercise 1.12: Pascal triangle
(define (pascal r c)
  (cond ((or (< r 1) (< c 1) (> c r)) 0)
        ((or (= c 1) (= c r)) 1)
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))
(pascal 1 0)                            ;0
(pascal 1 2)                            ;0
(pascal 1 1)                            ;1
(pascal 2 1)                            ;1
(pascal 2 2)                            ;1
(pascal 3 2)                            ;2
(pascal 3 3)                            ;1
(pascal 5 3)                            ;6



;; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 12.5 / 3 / 3 / 3 / 3 / 3 = 0.051440329218 < 0.1
;; -> p is called 5 times to compute (sine 12.5) -- confirmation:
;; scheme@(guile-user) [1]> ,trace (sine 12.5)

;; memory, operations ~ invocations of p
;; angle * (1/3)**invocations <= 0.1
;; ->
;; invocations ~ ceil(ln(10 * angle) / ln(3))
;;
;; O(log(angle))


;; 1.2.4 Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1))))) ; O(n) space, O(n) steps

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))       ; O(1) space, O(n) steps

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1)))))) ; O(log n) space and steps
(define (square a)
  (* a a))
(define (even? n)
  (= (remainder n 2) 0))

;; Exercise 1.16
;;
;; [n odd]
;; b^n = a b^n        , a = 1, base=b
;;     = a b^(n-1)    , a = b, base=b
;;     = a (b^2)^((n-1)/2),    base=b^2
;;
;; a accumulates factors until it is the result,
;; base will be squared again and again,
;; exponent will be halved or decremented as necessary
(define (fast-expt-i b n)
  (define (fast-expt-iter a base exponent)
    (cond ((= exponent 0) a)
          ((not (even? exponent))
           (fast-expt-iter (* a base) base (- exponent 1)))
          (else (fast-expt-iter a (square base) (/ exponent 2)))))
  (fast-expt-iter 1 b n))


;; Exercise 1.17
(define (my* a b)
  (if (= b 0)
      0
      (+ a (my* a (- b 1)))))
(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (fast-my* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-my* a (halve b))))
        (else (+ a (fast-my* a (- b 1))))))

;; Exercise 1.18
(define (fast-my*-i a b)
  (define (*iter x a b)
    (cond ((= 0 b) x)
          ((not (even? b)) (*iter (+ x a) a (- b 1)))
          (else (*iter x (double a) (halve b)))))
  (*iter 0 a b))
