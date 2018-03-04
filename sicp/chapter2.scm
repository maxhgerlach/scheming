;; Chapter 2: Building Abstractions with Data

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; make-rat improvement 1: used gcd
; (define (make-rat n d)
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))


;; Exercise 2.1
;; make-rat improvement 2: handle signs
(define (make-rat n d)
  (let ((g (gcd n d)))                  ;this would be neater with let*
    (let ((signed-d (/ d g)))
      (let ((new-d (abs signed-d)))
        (let ((sign-d (/ signed-d new-d)))
          (cons (* sign-d (/ n g)) new-d))))))

;; Exercise 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (sub-point p1 p2)
  (make-point (- (x-point p1) (x-point p2))
              (- (y-point p1) (y-point p2))))
(define (add-point p1 p2)
  (make-point (+ (x-point p1) (x-point p2))
              (+ (y-point p1) (y-point p2))))
(define (div-point p scalar)
  (make-point (/ (x-point p) scalar)
              (/ (y-point p) scalar)))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (add-point (start-segment seg)
             (div-point (sub-point (end-segment seg) (start-segment seg))
                        2)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))


;; Exercise 2.3 (rectangles) skipped


;; Exercise 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))

;; (my-car (my-cons 1 2))
;; (my-car (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) p))
;; ((lambda (p q) p) 1 2)
;; 1

(define (my-cdr z)
  (z (lambda (p q) q)))

;; (my-cdr (my-cons 1 2))
;; 2


;; Exercise 2.5 [uses float representation]
(define (power-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (power-car x)
  (if (= 0 (remainder x 3))
      (power-car (/ x 3))
      (/ (log x) (log 2))))
(define (power-cdr x)
  (if (= 0 (remainder x 2))
      (power-cdr (/ x 2))
      (/ (log x) (log 3))))


;; Exercise 2.6 Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))


(define (int-to-church n) 
  (define (iter a result) 
    (if (> a n) 
        zero 
        (add-1 (iter (+ a 1) result)) 
        )) 
  (iter 1 zero)) 

(define (church-to-int cn) 
  ((cn (lambda (n) (+ n 1))) 0))

(define (add-some a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; (church-to-int (add-some two (add-some one two)))
;; $26 = 5


;; 2.1.4 Extended Exercise: Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

;; (mul-interval (make-interval 10 12) (make-interval 8 9)) ;(80 . 108)
;; (add-interval (make-interval 10 12) (make-interval 8 9)) ;(18 . 21)

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
;; (sub-interval (make-interval 10 12) (make-interval 8 9)) ; (1 . 4)
;; (sub-interval (make-interval 10 12) (make-interval -8 9)) ; (1 . 20)


;; Exercise 2.9
(define (width interval) (- (upper-bound interval) (lower-bound interval)))

;; (width (add-interval x y))
;; = (width 
;;    (make-interval (+ (lower-bound x) (lower-bound y))
;;                   (+ (upper-bound x) (upper-bound y))))
;; = (- (+ (upper-bound x) (upper-bound y))
;;      (+ (lower-bound x) (lower-bound y)))
;; = (+ (- (upper-bound x) (lower-bound x))
;;      (- (upper-bound y) (lower-bound y)))
;; = (+ (width x) (width y))
;;
;; (width (sub-interval x y))
;; = (width (add-interval x (- y)))
;; = (+ (width x) (width (-y)))
;; = (- (width x) (width y))
;;
;; (mul-interval (make-interval -1 1) (make-interval 10 11)) = (-11, 11)     | width is 22
;; (mul-interval (make-interval -1 1) (make-interval 100 101)) = (-101, 101) | width is 202
;;
;; similar for division


;; Excercise 2.10
(define (div-interval x y)
  (if (>= 0 (* (upper-bound y) (lower-bound y)))
      (error "Division by interval crossing zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))


;; skip ex. 2.11

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(define (make-center-percent c p)
  (let ((w (abs (* c (/ p 100)))))
    (make-center-width c w)))
(define (percent i)
  (* 100 (abs (/ (width i) (center i)))))


;; skipping ex. 2.13...16


;; Chapter 2.2.1
;; nil is no longer in Scheme, use '()


;; Exercise 2.17
;; (define (last-pair l)
;;   (let ((len (length l)))
;;     (cond ((<= len 1) l)                  ;will return '() if l is empty, else last element
;;           (else (last-pair (cdr l))))))
;; calling length repeatedly is not very efficient, better:
(define (last-pair l)
  (define (last-pair-non-nil l)
    (let ((rest (cdr l)))
      (if (null? rest)
          l
          (last-pair-non-nil rest))))
  (if (null? l)
      l
      (last-pair-non-nil l)))
            
      
;; Exercise 2.18
(define (reverse l)
  (define (reversor l1 l2)
    (if (null? l1)
        l2
        (reversor (cdr l1) (cons (car l1) l2))))
  (reversor l '()))


;; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; scheme@(guile-user)> (cc 100 us-coins)
;; $23 = 292
;; scheme@(guile-user)> (cc 100 uk-coins)
;; $24 = 104561


;; Exercise 2.20
(define (same-parity starter . arguments)
  (define (take-these f? arguments)
    (cond ((null? arguments) '())
          ((f? (car arguments)) (cons (car arguments) (take-these f? (cdr arguments))))
          (else (take-these f? (cdr arguments)))))
  (cons starter (if (even? starter)
                    (take-these even? arguments)
                    (take-these odd? arguments))))



;; Mapping over lists
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; Exercise 2.21
(define (square x) (* x x))
(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map square items))


;; Exercise 2.22
