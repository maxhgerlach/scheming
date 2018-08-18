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


;; Exercise 2.41
(define (unique-triples n)
  (flatmap
   (lambda (m)
     (map (lambda (pair) (cons m pair))
          (unique-pairs (- m 1))))
   (enumerate-interval 3 n)))

;; (unique-triples 5)
;; => ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))


;; Exercise 2.42
;; 8 Queens

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (queens board-size)
  (define empty-board '())

  ;; storing the col is actually redundant
  (define (make-position row col)
    (cons row col))
  (define (position-row position)
    (car position))
  (define (position-col position)
    (cdr position))

  ;; alternatively we could just have the latest added col in the car of the list positions
  (define (adjoin-position row col positions)
    (append positions (list (make-position row col))))
  
  (define (safe? col positions)
    (define (same-row? p1 p2)
      (= (position-row p1) (position-row p2)))
    (define (same-diagonal? p1 p2)
      (= (abs (- (position-row p1) (position-row p2)))
         (abs (- (position-col p1) (position-col p2)))))
    (let ((this-queen (list-ref positions (- col 1)))
          (other-queens (filter (lambda (other-pos)
                                  (not (= col (position-col other-pos))))
                                positions)))
      (define (this-checks? some-other-queens)
        (if (null? some-other-queens)
            #f
            (or (same-row? this-queen (car some-other-queens))
                (same-diagonal? this-queen (car some-other-queens))
                (this-checks? (cdr some-other-queens)))))
      (not (this-checks? other-queens))))
    
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; scheme@(guile-user)> (queens 1)
;; $29 = (((1 . 1)))
;; scheme@(guile-user)> (queens 2)
;; $30 = ()
;; scheme@(guile-user)> (queens 3)
;; $31 = ()
;; scheme@(guile-user)> (queens 4)
;; $32 = (((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))
;; scheme@(guile-user)> (length (queens 5))
;; $34 = 10
;; scheme@(guile-user)> (length (queens 8))
;; $37 = 92



;; Exercise 2.43

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; This would call the expensive queens-cols function board-size times
;; each time a column is added. Since this is tree recursive, there
;; will be exponential growth in run time.


;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3 Symbolic data ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define true #t)
(define false #f)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


;; Exercise 2.53

(list 'a 'b c)        ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2)))                ; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))               ; (y1 y2)
(pair? (car '(a short list)))           ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks))     ; (red shoes blue socks)


;; Exercise 2.54

(define (my-equal? l1 l2)
  (cond ((and (not (pair? l1))
              (not (pair? l2)))
         (eq? l1 l2))
        ((and (pair? l1) (pair? l2))
         (and (my-equal? (car l1) (car l2))
              (my-equal? (cdr l1) (cdr l2))))
        (else false)))

(my-equal? '(this is a list) '(this is a list)) ; #t
(my-equal? '(this is a list) '(this is a )) ; #f
(my-equal? '(this is a list) '(this (is a) list)) ; #f


;; Exercise 2.55

(car ''abracadabra)                     ;quote
(car (quote 'abracadabra))              ;quote
(car (quote (quote abracadabra)))       ;quote



;; 2.3.2 Example: Symbolic Differentiation
;;
;; Exercise 2.56, 2.57


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? var exp)
                             1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)          ;does not handle x**x correctly
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (make-sum (exponent exp) (- 1))))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (accumulate make-sum 0 (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (accumulate make-product 1 (cddr p)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p))
         (expt b p))
        (else (list '** b p))))
        



;; examples

(deriv '(+ x 3) 'x)                     ; (+ 1 0) ; 1
(deriv '(* x y) 'x)                     ; (+ (* x 0) (* y 1)) ; y
(deriv '(* (* x y) (+ x 3)) 'x)         ; (+ (* x y) (* y (+ x 3)))

;; example Ex. 2.56

(deriv '(** x 4) 'x)                         ; (* 4 (** x 3))
(deriv '(+ (* 2 (* a x)) b) 'x)         ; (* 2 a)
(deriv '(+ (+ (* 2 (* a x)) b) c) 'x)   ; (* 2 a)
(deriv '(+ (+ (* a (** x 2)) b) c) 'x)  ; (* a (* 2 x))
(deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x) ; (+ (* a (* 2 x)) b)

;; example Ex. 2.57

(deriv '(* x y (+ x 3)) 'x)             ; (+ (* x y) (* y (+ x 3)))
(deriv '(* x y (+ x 3 4 5)) 'x)         ; (+ (* x y) (* y (+ x 3 4 5)))


;; example Ex. 2.58 part a in separate file, skipping part b for now


;; 2.3.3 Example: Representing Sets
;;
;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; example
(let* ((s (adjoin-set 'a '()))
       (ss (adjoin-set 'b s)))
  (element-of-set? 'b ss))              ; #t

(intersection-set '(1 2 3 4 5 8) '(9 10 3 4 2)) ; (2 3 4)


;; Exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(union-set '(1 2 3 4 5 8) '(9 10 3 4 2)) ; => (8 5 1 9 10 3 4 2)
(union-set '(1 2 3 4 5 8) '())           ; => (8 5 4 3 2 1)
(union-set '() '(1 2 3 4 5 8))           ; => (1 2 3 4 5 8)
(union-set '() '())                      ; => ()


;; Exercise 2.60

(define (dup-element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (dup-element-of-set? x (cdr set)))))

(dup-element-of-set? 1 '(2 3 2 1 3 2 2))
(dup-element-of-set? 3 '(2 3 2 1 3 2 2))

;; dup-element-of-set? should have the same asymptotic efficiency O(n)
;; as element-of-set?, however if it is unordered with many duplicates
;; of elements, searching for other elements may take longer, e.g., 1
;; in the set above.


(define (dup-adjoin-set x set)
  (cons x set))

(dup-adjoin-set 3 '(2 3 2 1 3 2 2))

;; dup-adjoin-set is O(1) now rather than O(n), of course memory usage is higher


(define (dup-union-set set1 set2)
  (cond ((null? set1) set2)
        (else (dup-union-set
               (cdr set1)
               (dup-adjoin-set (car set1) set2)))))

;; dup-union-set is O(n) rather than O(n^2) now

(dup-union-set '(1 2 3 4 5 8) '(9 10 3 4 2)) ; => (8 5 4 3 2 1 9 10 3 4 2)


(define (dup-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((dup-element-of-set? (car set1) set2)
         (cons (car set1) (dup-intersection-set (cdr set1) set2)))
        (else (dup-intersection-set (cdr set1) set2)))) ; O(n^2)

(dup-intersection-set '(1 2 3 4 5 8) '(9 10 3 4 2)) ; (2 3 4)
(dup-intersection-set '(1 2 3 2 2 4 3 5 8) '(2 3 2 1 3 2 2)) ; (1 2 3 2 2 3)



;; Sets as ordered lists

(define (ord-element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (ord-element-of-set? x (cdr set))))) ; O(n), but n/2

(ord-element-of-set? 6 '(1 3 6 10))
(ord-element-of-set? 10 '(1 3 6 10))
(ord-element-of-set? 4 '(1 3 6 10))
(ord-element-of-set? 12 '(1 3 6 10))


(define (ord-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (ord-intersection-set (cdr set1)
                                              (cdr set2))))
              ((< x1 x2)
               (ord-intersection-set (cdr set1) set2))
              ((< x2 x1)
               (ord-intersection-set set1 (cdr set2))))))) ; O(n)


(ord-intersection-set '(1 2 3 4 5 8) '(2 3 4 9 10)) ; (2 3 4)
(ord-intersection-set '(1 2 3 4 5 8) '(1 2 3))      ; (1 2 3)


;; Exercise 2.61

(define (ord-adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set)
                               (ord-adjoin-set x (cdr set)))))) ; O(n), but n/2

(ord-adjoin-set 3 '(1 2 3))             ; (1 2 3)
(ord-adjoin-set 0 '(1 2 3))             ; (0 1 2 3)
(ord-adjoin-set 8 '(1 2 3 10))          ; (1 2 3 8 10)
(ord-adjoin-set 8 '(10 12))             ; (8 10 12)
(ord-adjoin-set 8 '())                  ; (8)
(ord-adjoin-set 8 '(1 2 5))             ; (1 2 5 8)


;; Exercise 2.62

(define (ord-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (ord-union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (ord-union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (ord-union-set set1 (cdr set2))))))))) ; O(n)

(ord-union-set '(1 2 3 4 5 8) '(2 3 4 9 10)) ; => (1 2 3 4 5 8 9 10)
(ord-union-set '(1 2 3 4 5 8) '())           ; => (8 5 4 3 2 1)
(ord-union-set '() '(1 2 3 4 5 8))           ; => (1 2 3 4 5 8)
(ord-union-set '() '())                      ; => ()



;; Sets as binary trees
;;;;;;;;;;;;;;;;;;;;;;;


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; Exercise 2.63

;; Trees from figure 2.16 (representing the set {1,3,5,7,9,11}

(define t1 '(7 (3 (1 () ())
                  (5 () ()))
               (9 ()
                  (11 () ()))))
(define t2 '(3 (1 () ())
               (7 (5 () ())
                  (9 ()
                     (11 () ())))))
(define t3 '(5 (3 (1 () ())
                  ())
               (9 (7 () ())
                  (11 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(tree->list-1 t1)                       ; => (1 3 5 7 9 11)
(tree->list-1 t2)                       ; => (1 3 5 7 9 11)
(tree->list-1 t3)                       ; => (1 3 5 7 9 11)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(tree->list-2 t1)                       ; => (1 3 5 7 9 11)
(tree->list-2 t2)                       ; => (1 3 5 7 9 11)
(tree->list-2 t3)                       ; => (1 3 5 7 9 11)


;; Ex. 2.63.a
;;
;; tree->list-1 and tree->list-2 yield the same results (both traverse
;; the tree in the same order)
;;
;;
;;
;; Ex. 2.63.b
;;
;; See handwritten calculations:
;; 
;; tree->list-1 is O(n log n) since append has linear complexity
;;
;; tree->list-2 is O(n) since cons has linear complexity


;; Ex. 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                   (cons (make-tree this-entry
                                    left-tree
                                    right-tree)
                         remaining-elts))))))))

(list->tree '(1 3 5 7 9 11)) ;; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;                              
;;            5                 
;;           / \                
;;          /   \               
;;         1     9         
;;          \   / \                      
;;          3  7   11
;;
;;
;; (partial-tree elements (length elements)) builds a tree from all elements
;;
;; If elements are not ordered, (partial-tree elements n) may not
;; produce a balanced tree. ...
;;
;;
;; O(n)


;; Ex. 2.65 -- making use of previous routines to remain O(n)

(define (tree-union-set tree1 tree2)
  (let ((list1 (tree->list-2 tree1))
        (list2 (tree->list-2 tree2)))
    (let ((list-union (ord-union-set list1 list2)))
      (list->tree list-union))))

(list->tree '(1 2 3 4 5 8))             ; (3 (1 () (2 () ())) (5 (4 () ()) (8 () ())))
(list->tree '(2 3 4 9 10))              ; (4 (2 () (3 () ())) (9 () (10 () ())))
(tree-union-set '(3 (1 () (2 () ())) (5 (4 () ()) (8 () ())))
                '(4 (2 () (3 () ())) (9 () (10 () ()))))
                                        ; (4 (2 (1 () ()) (3 () ())) (8 (5 () ()) (9 () (10 () ()))))
(tree->list-2 '(4 (2 (1 () ()) (3 () ())) (8 (5 () ()) (9 () (10 () ()))))) ; (1 2 3 4 5 8 9 10)



(define (tree-intersection-set tree1 tree2)
  (let ((list1 (tree->list-2 tree1))
        (list2 (tree->list-2 tree2)))
    (let ((list-intersection (ord-intersection-set list1 list2)))
      (list->tree list-intersection))))

(list->tree '(1 2 3 4 5 8))             ; (3 (1 () (2 () ())) (5 (4 () ()) (8 () ())))
(list->tree '(2 3 4 9 10))              ; (4 (2 () (3 () ())) (9 () (10 () ())))
(tree-intersection-set '(3 (1 () (2 () ())) (5 (4 () ()) (8 () ())))
                       '(4 (2 () (3 () ())) (9 () (10 () ())))) ; (3 (2 () ()) (4 () ()))
(tree->list-2 '(3 (2 () ()) (4 () ())))                         ; (2 3 4)


;; Ex. 2.66 -- set implemented as binary tree

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))


;;
;;
;; Chapter 2.3.4 Huffman encoding trees
;;
;;


(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
; the result of make-leaf-set is ordered: lowest weights come first

;(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))  ; ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

;; Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)     ; (A D A B B C A)

;; sample-tree
;; ((leaf A 4)
;;  ((leaf B 2)
;;   ((leaf D 1)
;;    (leaf C 1)
;;    (D C) 2)
;;   (B D C) 4)
;;  (A B D C) 8)




;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; symbols are ordered by their weights which are however not included
;; in the set: cannot directly use ord-element-of-set?
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (encode-symbol symbol tree)
  (define (build symbol tree bit-list)
    (define (progress new-symbol selected-branch)
      (let ((new-bit-list (append bit-list (list new-symbol))))
        (if (leaf? selected-branch)
            new-bit-list
            (build symbol selected-branch new-bit-list))))
    (cond ((element-of-set? symbol (symbols (left-branch tree)))
           (progress 0 (left-branch tree)))
          ((element-of-set? symbol (symbols (right-branch tree)))
           (progress 1 (right-branch tree)))
          (else (error "Symbol not in tree" symbol))))
  (build symbol tree '()))


;(encode '(A D A B B C A) sample-tree)   ;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;(encode '(A D A B E B C A) sample-tree) ;error




;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (define (successive-merge tree-set)
    (let ((lowest (car tree-set))
          (others (cdr tree-set)))
      (if (null? others)
          lowest
          (let ((next-lowest (car others))
                (remaining (cdr others)))
            (successive-merge (adjoin-set (make-code-tree lowest next-lowest)
                                          remaining))))))
  (successive-merge (make-leaf-set pairs)))

;; sample-tree
;; ((leaf A 4)
;;  ((leaf B 2)
;;   ((leaf D 1)
;;    (leaf C 1)
;;    (D C) 2)
;;   (B D C) 4)
;;  (A B D C) 8)
;;  
;; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;; ((leaf A 4)
;;  ((leaf B 2)
;;   ((leaf D 1)
;;    (leaf C 1)
;;    (D C) 2)
;;   (B D C) 4)
;;  (A B D C) 8)


;; Ex. 2.70

(define song-alphabet
  '((A 2) (GET 2) (SHA 3) (WAH 1)
    (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define song-tree
  (generate-huffman-tree song-alphabet))
(define song
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(define song-bits
  (encode song song-tree))

(length song-bits)                      ; 84 bits in the song-tree Huffman encoding

(length song)                           ; 36 symbols in the song
(length song-alphabet)                  ; 8 symbols in the alphabet

;; A fixed-length code for an eight-symbol alphabet would allocate ld
;; 8 = 3 bits per symbol.  So encoding the 36-symbol song would
;; require 3 * 36 = 108 bits.  We saved 24 bits.



;; Exercise 2.71
;;
;; 1 bit for the most frequent, n-1 for the two least frequent symbols.
