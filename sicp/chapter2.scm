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
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;; Exercise 2.1
;; make-rat improvement 2: handle signs
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((signed-d (/ d g)))
      (let ((new-d (abs signed-d)))
        (let ((sign-d (/ signed-d new-d)))
          (cons (* sign-d (/ n g)) new-d))))))
