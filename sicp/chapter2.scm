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
