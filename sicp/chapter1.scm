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
;(define (p) (p))
;(p) ;; infinite loop
;(define (test x y)
;  (if (= x 0) 0 y))

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
;(define (inc a) (+ a 1))
;(define (dec a) (- a 1))
;; I: a linear recursive procedure (growing chain of inc's)
; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))

; (+ 4 5)
; (if (= 4 0) 5 (inc (+ (dec 4) 5)))
; (if #f 5 (inc (+ 3 5)))
; (if #f 5 (inc (if (= 3 0) 5 (inc (+ (dec 3) 5)))))
; (if #f 5 (inc (if #f 5 (inc (+ 2 5)))))
; (if #f 5 (inc (if #f 5 (inc (if (= 2 0) 5 (inc (+ (dec 2) 5)))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (+ 1 5)))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if (= 1 0) 5 (inc (+ (dec 1) 5))) ))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (+ 0 5)))))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #t 5 (inc (+ (dec 0) 5)))))))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc 5))))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc (if #f 5 6)))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 (inc 6))))))
; (if #f 5 (inc (if #f 5 (inc (if #f 5 7)))))
; (if #f 5 (inc (if #f 5 (inc 7))))
; (if #f 5 (inc (if #f 5 8)))
; (if #f 5 (inc 8))
; (if #f 5 9)
; 9
;; actually the (if #f ..) would be simplified earlier

;; II: iterative -- discarding the if's the number of variables to hold state does not grow indefinitely
; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))

; (+ 4 5)
; (if (= 4 0) 5 (+ (dec 4) (inc 5)))
; (if #f 5 (+ (dec 4) (inc 5)))
; (if #f 5 (+ 3 6))
; (if #f 5 (if (= 3 0) 6 (+ (dec 3) (inc 6))))
; (if #f 5 (if #f 6 (+ 2 7)))
; (if #f 5 (if #f 6 (if (= 2 0) 7 (+ (dec 2) (inc 7)))))
; (if #f 5 (if #f 6 (if #f 7 (+ 1 8))))
; (if #f 5 (if #f 6 (if (= a 1) 8 (+ (dec 1) (inc 8)))))
; (if #f 5 (if #f 6 (if #f 8 (+ 0 9))))
; (if #f 5 (if #f 6 (if #f 8 (if (= 0 0) 9 (+ (dec 0) (inc 9))))))
; (if #f 5 (if #f 6 (if #f 8 (if #t 9 (+ -1 10)))))
; (if #f 5 (if #f 6 (if #f 8 9)))
; 9


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

;; Exercise 1.19 : Fibonacci in logarithmic time
;; T^2_{p,q} = T_{p',q'} with p' = pp+qq, q' = qq+2pq
(define (fast-fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))         ; compute p ′
                     (+ (* q q) (* 2 p q))      ; compute q ′
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))


;; 1.2.5 Greatest Common Divisors

;; euclids algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.20
;;
;; normal order, directly expanding (if):
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)            ;* 1
    a
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0) ;** 2
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ;**** 4
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ;******* 7
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ;**** 4
2
;;
;; 1 + 2 + 4 + 7 + 4 = 18 remainder operations
;;
;; applicative order
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))             ;* 1
(gcd 40 6)
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))
(gcd 6 (remainder 40 6))                ;* 1
(gcd 6 4)
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))
(gcd 4 (remainder 6 4))                 ;* 1
(gcd 4 2)
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
(gcd 2 (remainder 4 2))                 ;* 1
(gcd 2 0)
(if (= 0 0)
    2
    (gcd 2 (remainder 2 0)))
2
;;
;; 4 remainder operations


;; 1.2.6 Testing for primality
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define true #t)
(define false #f)
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;; exercise 1.21
(smallest-divisor 199)                  ;199
(smallest-divisor 1999)                 ;1999
(smallest-divisor 19999)                ;7


;; exercise 1.22
(define (runtime) (tms:clock (times)))

(define (timed-prime-test n)
  ;; (newline)
  (display n)
  (start-prime-test n (runtime))
  (newline))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (next-odd int)
  (if (even? int) (+ int 1) (+ int 2)))
(define (announce-prime number start-time)
  (display number)
  (report-prime (- (runtime) start-time))
  (newline)
  number)
(define (examine beg start-time)
  (if (prime? beg)
      (announce-prime beg start-time)
      (examine (next-odd beg) (runtime))))
(define (search-n-primes beg n)
  (if (<= n 0) (display "done\n")
      (search-n-primes (next-odd (examine beg (runtime)))
       (- n 1))
      ))
(define (search-for-primes start)
  (search-n-primes start 3))

;;;; Increasing the number range by a factor of 10 approximately
;;;; increases the runtime by a factor of 3. ~ sqrt(10) -> sqrt
;;;; behavior, for large numbers only
;;;;
;; scheme@(guile-user)> (search-for-primes 1000000000)
;; 1000000007 *** 10000000
;; 1000000009 *** 30000000
;; 1000000021 *** 10000000
;; done
;; scheme@(guile-user)> (search-for-primes 10000000000)
;; 10000000019 *** 40000000
;; 10000000033 *** 30000000
;; 10000000061 *** 50000000
;; done
;; scheme@(guile-user)> (search-for-primes 100000000000)
;; 100000000003 *** 140000000
;; 100000000019 *** 130000000
;; 100000000057 *** 120000000
;; done
;; scheme@(guile-user)> (search-for-primes 1000000000000)
;; 1000000000039 *** 450000000
;; 1000000000061 *** 470000000
;; 1000000000063 *** 410000000


;; Exercise 1.23
(define (smallest-divisor n)
  (define (next num)                    ;skip all even test-divisors larger than 2
    (if (= num 2) 3 (+ num 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

;;;; times are now *lower than half* -- was something strange going on before in the old tests?

;; scheme@(guile-user)> (search-for-primes 1000000000)
;; 1000000007 *** 10000000
;; 1000000009 *** 10000000
;; 1000000021 *** 10000000
;; done
;; scheme@(guile-user)> (search-for-primes 10000000000)
;; 10000000019 *** 10000000
;; 10000000033 *** 20000000
;; 10000000061 *** 10000000
;; done
;; scheme@(guile-user)> (search-for-primes 100000000000)
;; 100000000003 *** 50000000
;; 100000000019 *** 40000000
;; 100000000057 *** 50000000
;; done
;; scheme@(guile-user)> (search-for-primes 1000000000000)
;; 1000000000039 *** 120000000
;; 1000000000061 *** 120000000
;; 1000000000063 *** 100000000
;; done


;; Exercise 1.24
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 1000)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  ;; (newline)
  (display n)
  (start-prime-test n (runtime))
  (newline))

;; scheme@(guile-user)> (timed-prime-test 1000000007)
;; 1000000007    *** 20000000
;; scheme@(guile-user)> (timed-prime-test 1000000000039)
;; 1000000000039 *** 60000000


;; Exercise 1.25
;;
;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;;
;; expmod with intermediate computations computed module
;; m will have to deal with smaller intermediate numbers. Arithmetics
;; with very large numbers are slow.


;; Exercise 1.26
;;
;; (define (expmod base exp m)
;; (cond ((= exp 0) 1)
;;       ((even? exp)
;;        (remainder (* (expmod base (/ exp 2) m)    ;; square replaced
;;                      (expmod base (/ exp 2) m))
;;                   m))
;;       (else
;;        (remainder (* base
;;                      (expmod base (- exp 1) m))
;;                   m))))
;;
;; By doing the computation (expmod base (/ exp 2) m) twice the
;; speed-up from halving the exponent is canceled. O(log n) -> O(n)



;; Chapter 1.3.1
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;; scheme@(guile-user)> (integral cube 0 1 0.01)
;; $6 = 0.24998750000000042
;; scheme@(guile-user)> (integral cube 0 1 0.001)
;; $7 = 0.249999875000001


;; Exercise 1.29
(define (simp-integral f a b n)
  (define h (/ (- b a) n))
  (define (next-k k) (+ k 1))
  (define (simp-term k)
    (* (f (+ a (* k h)))
       (cond ((= 0 k) 1)
             ((= n k) 1)
             ((even? k) 2)
             (else 4))))
  (* (/ h 3)
     (sum simp-term 0 next-k n)))

;; scheme@(guile-user)> (simp-integral cube 0. 1. 100)
;; $4 = 0.24999999999999992
;; scheme@(guile-user)> (simp-integral cube 0. 1. 1000)
;; $5 = 0.2500000000000003


;; Exercise 1.30
;; iterative sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; Exercise 1.31
;;
;; product here implemented as an iterative proces

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (wallis-pi steps)
  (define (pi-factor k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))
  (* 4. (product pi-factor 1 inc steps)))

;; scheme@(guile-user) [1]> (wallis-pi 10)
;; $11 = 3.2751010413348074
;; scheme@(guile-user) [1]> (wallis-pi 100)
;; $12 = 3.1570301764551676
;; scheme@(guile-user) [1]> (wallis-pi 1000)
;; $13 = 3.1431607055322663

(define (product-rec term a next b)
  (if (> a b) 1
      (* (term a) (product-rec term (next a) next b))))
(define (factorial-rec n)
  (product-rec identity 1 inc n))



;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; (accumulate * 1 identity 1 inc 69)
;; $17 = 171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000

(define (acc-product term a next b)
  (accumulate * 1 term a next b))
(define (acc-sum term a next b)
  (accumulate + 0 term a next b))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

;; (accumulate-rec * 1 identity 1 inc 69)
;; $18 = 171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000



;; Exercise 1.33

(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter? a) (term a) null-value)))))
  (iter a null-value))

;; scheme@(guile-user) [3]> (filtered-accumulate even? + 0 identity 0 inc 10)
;; $19 = 30
;; scheme@(guile-user) [3]> (+ 2 4 6 8 10)
;; $20 = 30


(define (sum-of-squared-primes a b)
  (define (prime-not-1? x)              ;proper def. of a prime
    (and (> x 1) (prime? x)))
  (filtered-accumulate prime-not-1? + 0 square a inc b))

;; scheme@(guile-user) [3]> (sum-of-squared-primes 1 5)
;; $24 = 38

(define (product-of-relatively-prime-smaller n)
  (define (pred? i)
    (= 1 (gcd i n)))
  (filtered-accumulate pred? * 1 identity 1 inc (- n 1)))

;; (product-of-relatively-prime-smaller 10)
;; $27 = 189



;; Exercise 1.34

(define (f g) (g 2))
;; (f f)
;; ERROR: In procedure 2:
;; ERROR: Wrong type to apply: 2

;; recursive application 
;(f f)
;(f 2)
;(2 2) ; error



;; 1.3.3 Procedures as General Methods
;;
;;
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;; Exercise 1.35
;;
;; golden ratio phi^2 = phi + 1 -> phi = 1 + 1/phi

(fixed-point (lambda (x) (+ 1. (/ 1. x))) 1.0) ; 1.6180327868852458


;; Exercise 1.36

(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (verbose-fixed-point (lambda (x) (+ 1. (/ 1. x))) 1.0)
(verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5) ;35 steps until 4.555539351985717
(verbose-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.5) ;11 steps until 4.555537952796512


;; Exercise 1.37

(/ 1 (fixed-point (lambda (x) (+ 1. (/ 1. x))) 1.0)) ;=> 0.6180344478216819 is 1/phi

(define (cont-frac n d k)
  (define (sub-frac i)
    (/ (n i) (+ (d i) (if (>= i k)
                          0
                          (sub-frac (+ i 1))))))
  (sub-frac 1))


(define (show-cont-fracs k-start k-end)
  (define (step k)
    (let ((cf (cont-frac (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k)))
      (display cf)
      (newline)
      (if (> k k-end)
          cf
          (step (+ k 1)))))
  (step 1))
    
;; scheme@(guile-user) [3]> (show-cont-fracs 1 10)
;; 1.0
;; 0.5
;; 0.6666666666666666
;; 0.6000000000000001
;; 0.625
;; 0.6153846153846154
;; 0.6190476190476191
;; 0.6176470588235294
;; 0.6181818181818182
;; 0.6179775280898876
;; 0.6180555555555556




;; Exercise 1.38

(+ (cont-frac (lambda (i) 1.0)
              (lambda (i) (if (= 0 (remainder (+ i 1) 3))
                              (* 2. (/ (+ i 1) 3))
                              1.0))
              10) 2)                    ; => 2.7182817182817183



;; Exercise 1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

;; scheme@(guile-user) [1]> (tan-cf 0.5 10)
;; $13 = 0.5463024898437905
;; scheme@(guile-user) [1]> (tan 0.5)
;; $14 = 0.5463024898437905
