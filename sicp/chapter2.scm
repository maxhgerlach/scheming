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
(define (square-list-iter1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; In ech iteration step, we square the first element of things, put
;; it to the start of answer, then throw away the first element of
;; things. This reverses the list.

(define (square-list-iter2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; cons'ing in this way does not build a proper list.


;; Exercise 2.23
(define (my-for-each f lst)
  (cond ((null? lst) #t)
        (else (f (car lst))
              (my-for-each f (cdr lst)))))


;; Hierarchical Structures
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.25

;; (define x (list 1 3 (list 5 7) 9))
;; (car (cdr (car (cdr (cdr x)))))
;; $61 = 7

;; (car (car (list (list 7))))
;; $62 = 7

;; (define y (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; (cadr (cadr (cadr (cadr (cadr (cadr y))))))
;; $72 = 7


;; Exercise 2.26
;; (append x y)
;; $73 = (1 2 3 4 5 6)
;; scheme@(guile-user)> (cons x y)
;; $74 = ((1 2 3) 4 5 6)
;; scheme@(guile-user)> (list x y)
;; $75 = ((1 2 3) (4 5 6))


;; Exercise 2.27
(define (deep-reverse l)
  (define (reversor l1 l2)
    (if (null? l1)
        l2
        (reversor (cdr l1)
                  (cons (deep-reverse (car l1)) l2))))
  (if (pair? l)
      (reversor l '())
      l))


;; Exercise 2.28
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))


;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  ;; length: number
  ;; structure: mobile or number
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (structure-weight (branch-structure (left-branch mobile)))
     (structure-weight (branch-structure (right-branch mobile)))))

(define (structure-weight structure)
  (if (not (pair? structure))
      structure
      (total-weight structure)))

;; (define mob 
;;   (make-mobile 
;;    (make-branch 4 10)
;;    (make-branch 5 
;;                 (make-mobile (make-branch 2 3)
;;                              (make-branch 8 10)))))
;; (total-weight mob)
;; $18 = 23

(define (structure-balanced? structure)
  (if (not (pair? structure))
      #t
      (mobile-balanced? structure)))

(define (branch-torque branch)
  (* (branch-length branch)
     (structure-weight (branch-structure branch))))

(define (mobile-balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (structure-balanced? (branch-structure (left-branch mobile)))
       (structure-balanced? (branch-structure (right-branch mobile)))))


;; new: (define (make-mobile left right) (cons left right))
;;      (define (make-branch length structure)
;;         (cons length structure))
;; necessary changes:
;; (define (right-branch mobile)
;;   (cdr mobile))
;; (define (branch-structure branch)
;;   (cdr branch))


;; Exercise 2.30
(define (square-tree-1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))


;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append
         rest (map
               (lambda (rest-el)
                 (cons (car s) rest-el))
               rest)))))


;; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-via-acc p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   '() sequence))
(define (append-via-acc seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-via-acc sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))


;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; (horner-eval 2 (list 1 3 0 5 0 1))      ;79


;; Exercise 2.35

;; (define (count-leaves x)
;;   (cond ((null? x) 0)
;;         ((not (pair? x)) 1)
;;         (else (+ (count-leaves (car x))
;;                  (count-leaves (cdr x))))))

(define (count-leaves-acc t)
  (accumulate + 0
              (map
               (lambda (x)
                 (cond ((null? x) 0)
                       ((not (pair? x)) 1)
                       (else (count-leaves-acc x))))
               t)))
;; (count-leaves-acc (cons (list 1 2 (list 3 '() (list 7 8 9) )) (list 3 4)))
;; $36 = 8


;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             '()
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

;; (define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;; scheme@(guile-user)> (map car s)
;; $37 = (1 4 7 10)
;; scheme@(guile-user)> (map cdr s)
;; $38 = ((2 3) (5 6) (8 9) (11 12))
;; scheme@(guile-user)> (accumulate-n + 0 s)
;; $39 = (22 26 30)


;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '()  mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))

;; (define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
;; scheme@(guile-user)> (define v (list 0 0 0 1))
;; scheme@(guile-user)> (define v (list 0 1 2 1))
;; scheme@(guile-user)> (matrix-*-vector m v)
;; $42 = (12 23 32)
;; scheme@(guile-user)> (transpose m)
;; $43 = ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
;; scheme@(guile-user)> (matrix-*-matrix m (transpose m))
;; $45 = ((30 56 80) (56 113 161) (80 161 230))


;; Exercise 2.38
(define fold-right accumulate)

;; (fold-right / 1 (list 1 2 3)) ; 3/2
;; 1, (/ 3 1)=3, (/ 2 3)=2/3, (/ 1 2/3)=3/2

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; (fold-left / 1 (list 1 2 3)) ; 1/6

(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))

(fold-left list '() (list 1 2 3)) ; (((() 1) 2) 3)


;; for commutative and associative op's fold-left and fold-right are
;; equivalent


;; Exercise 2.39

(define (reverse l)
  (define (reversor l1 l2)
    (if (null? l1)
        l2
        (reversor (cdr l1) (cons (car l1) l2))))
  (reversor l '()))

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;; (reverse-fr (list 1 2 3))               ; (3 2 1)

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x )) '() sequence))

;; (reverse-fl (list 1 2 3))               ; (3 2 1)


;; Nested mappings
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; (flatmap (lambda (i)
;;            (map (lambda (j) (list i j))
;;                 (enumerate-interval 1 (- i 1))))
;;          (enumerate-interval 1 10))
;; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5) (7 1) (7 2) (7 3) (7 4) (7 5) (7 6) (8 1) (8 2) (8 3) (8 4) (8 5) (8 6) (8 7) (9 1) (9 2) (9 3) (9 4) (9 5) (9 6) (9 7) (9 8) (10 1) (10 2) (10 3) (10 4) (10 5) (10 6) (10 7) (10 8) (10 9))

;; > (map (lambda (i) (list 0 i)) (list 1 2 3 4))
;; $7 = ((0 1) (0 2) (0 3) (0 4))
;; > (flatmap (lambda (i) (list 0 i)) (list 1 2 3 4))
;; $8 = (0 1 0 2 0 3 0 4)

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;; Exercise 2.40
(define (unique-pairs n)
  (if (< n 2)
      '()
      (flatmap
       (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 2 n))))

;; (unique-pairs 5)
;; $11 = ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


