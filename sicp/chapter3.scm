;; Chapter 3

;; Ex. 3.1
(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

;; scheme@(guile-user)> (define A (make-accumulator 0))
;; scheme@(guile-user)> (A 0)
;; $7 = 0
;; scheme@(guile-user)> (A 1)
;; $8 = 1
;; scheme@(guile-user)> (A 2)
;; $9 = 3
;; scheme@(guile-user)> (A -5)
;; $10 = -2
;; scheme@(guile-user)> (A 10)
;; $11 = 8


;; Ex. 3.2
(define (make-monitored fn)
  (let ((calls 0))
    (define (dispatch m)
      (if (eq? m 'how-many-calls?)
          calls
          (begin
            (set! calls (+ calls 1))
            (fn m))))
    dispatch))

;; scheme@(guile-user)> (define sqrt-mon (make-monitored sqrt))
;; scheme@(guile-user)> (sqrt-mon 10.0)
;; $13 = 3.1622776601683795
;; scheme@(guile-user)> (sqrt-mon 100.0)
;; $14 = 10.0
;; scheme@(guile-user)> (sqrt-mon 101.0)
;; $15 = 10.04987562112089
;; scheme@(guile-user)> (sqrt-mon 'how-many-calls?)
;; $16 = 3


;; Ex. 3.3, 3.4
(define (make-account balance stored-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-password-given 0))
    (define (call-the-cops) "Too many wrong passwords!")
    (define (checked password fn)
      (if (eq? password stored-password)
          (begin
            (set! wrong-password-given 0)
            (lambda (amount) (fn amount)))
          (begin
            (set! wrong-password-given (+ 1 wrong-password-given ))
            (if (> wrong-password-given 7)
                (lambda (_) (call-the-cops))
                (lambda (_) "Incorrect password")))))
    (define (dispatch password m)
      (cond ((eq? m 'withdraw) (checked password withdraw))
            ((eq? m 'deposit) (checked password deposit))
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

;; scheme@(guile-user)> (define A (make-account 100 'secret-password))
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 10)
;; $21 = "Incorrect password"
;; scheme@(guile-user)> ((A 'secret-password 'withdraw) 10)
;; $22 = 90
;; scheme@(guile-user)> ((A 'wrong-password 'deposit) 20)
;; $23 = "Incorrect password"
;; scheme@(guile-user)> ((A 'secret-password 'deposit) 20)
;; $24 = 110


;; scheme@(guile-user)> (define A (make-account 100 'secret-password))
;; scheme@(guile-user)> ((A 'secret-password 'withdraw) 1)
;; $25 = 99
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $26 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $27 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $28 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $29 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $30 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $31 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $32 = "Incorrect password"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $33 = "Too many wrong passwords!"
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 99)
;; $34 = "Too many wrong passwords!"
;; scheme@(guile-user)> ((A 'secret-password 'withdraw) 99)
;; $35 = 0



;; Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (P-random-point)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials P-random-point)
     (* (- x2 x1) (- y2 y1))))

(define (in-unit-circle x y)
  (<= (+ (* x x) (* y y)) 1))

(define pi-estimate (estimate-integral in-unit-circle -1.0 1.0 -1.0 1.0 100000))

;; pi-estimate                             ; => 3.14808


;; Ex. 3.7

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (make-account balance initial-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-password-given 0))
    (define (call-the-cops) "Too many wrong passwords!")
    (define (checked password fn)
      (if (eq? password initial-password)
          (begin
            (set! wrong-password-given 0)
            (lambda (amount) (fn amount)))
          (begin
            (set! wrong-password-given (+ 1 wrong-password-given ))
            (if (> wrong-password-given 7)
                (lambda (_) (call-the-cops))
                (lambda (_) "Incorrect password")))))
    (define (dispatch password m)
      (cond ((eq? m 'withdraw) (checked password withdraw))
            ((eq? m 'deposit) (checked password deposit))
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-joint acc initial-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
        (acc initial-password m)
        (error "Incorrect pasword for joint account")))
  dispatch)


;; Ex. 3.8

(define f
  (let ((call-1 0)
        (call-2 0))
    (lambda (n)
      (set! call-1 call-2)
      (set! call-2 n)
      call-1)))


;;
;; Chapter 3.3 : Modeling with Mutable Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ex. 3.12

(define (my-append x y)
  (if (null? x)
      y
      (cons (car x) (my-append (cdr x) y))))

(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

(define (my-last-pair x)
  (if (null? (cdr x)) x (my-last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (my-append x y))

;; z                                       ; (a b c d)

;; (cdr x)                                 ; (b)

(define w (my-append! x y))

;; w                                       ; (a b c d)

;; (cdr x)                                 ; (b c d)



;; Ex. 3.13

(define (make-cycle x)
  (set-cdr! (my-last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; (my-last-pair z) ;; does not halt
;; (last-pair z)   ;Error: Circular structure in position 1: (a b c . #-2#)

n
;; Ex. 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(mystery v)                             ; (d c b a)


;; Exercise 3.16

(define (broken-count-pairs x)
  (if (not (pair? x))
      0
      (+ (broken-count-pairs (car x))
         (broken-count-pairs (cdr x))
         1)))

(broken-count-pairs (list 1 2 3))       ; 3

(define x (cons 3 '()))
(broken-count-pairs (cons x (cons 2 x))) ; 4

(define y (cons x x))
(broken-count-pairs (cons y y))         ; 7


;; Exercise 3.17

(define (count-pairs x)
  (let ((seen '()))
    (define (count x)
      (cond ((not (pair? x))
             0)
            ((memq x seen)
             0)
            (else (begin
                    (set! seen (cons x seen))
                    (+ (count (car x))
                       (count (cdr x))
                       1)))))
    (count x)))

(count-pairs (list 1 2 3))              ; 3
(define x (cons 3 '()))
(count-pairs (cons x (cons 2 x)))       ; 3
(define y (cons x x))
(count-pairs (cons y y))                ; 3


;; Exercise 3.18
;; check "whether a program that tried to find the end of the list by
;; taking successive cdrs would go into an infinite loop."

(define (contains-cycle lst)
  (let ((seen '()))
    (define (check x)
      (cond ((not (pair? x)) #f)
            ((memq x seen) #t)
            (else
             (set! seen (cons x seen))
             (check (cdr x)))))
    (check lst)))
(contains-cycle (list 1 2 3))           ; #f

(define problem-list (list 1 2 3))
(set-cdr! (cddr problem-list) problem-list)
(contains-cycle problem-list)           ; #t


;; Exercise 3.19
;; Floyd's algorithm (tortoise and hare)

(define (contains-cycle-efficient lst)
  (define (iter slow fast)
    (cond ((eq? slow fast) #t)
          ((null? fast) #f)
          ((null? (cdr fast)) #f)
          (else (iter (cdr slow) (cddr fast)))))
  (if (or (null? lst) (null? (cdr lst)) (null? (cddr lst)))
      #f
      (iter (cdr lst) (cddr lst))))

(contains-cycle-efficient (list 1 2 3)) ; #f

(contains-cycle-efficient problem-list) ; #t

(define problem-list-4 (list 1 2 3 4))
(set-cdr! (cdddr problem-list-4) problem-list-4)
(contains-cycle-efficient problem-list-4) ; #t

(contains-cycle-efficient '())          ; #f



;; Chap. 3.3.2 Representing Queues


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-cons-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))


;; Exercise 3.21

(define (print-queue queue)
  (define (iter lst)
    (cond ((null? lst) '())
          (else
           (display (car lst))
           (display " ")
           (iter (cdr lst)))))
  (iter (front-ptr queue))
  (newline))


;; Exercise 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue"))
            (else (set! front-ptr (cdr front-ptr)))))
    (define (print)
      (define (iter lst)
        (cond ((null? lst) '())
              (else
               (display (car lst))
               (display " ")
               (iter (cdr lst)))))
      (iter front-ptr)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            ((eq? m 'print) print)))
    dispatch))

;; scheme@(guile-user)> (define q (make-queue))
;; scheme@(guile-user)> q
;; $6 = #<procedure dispatch (m)>
;; scheme@(guile-user)> (q 'empty?)
;; $7 = #<procedure empty? ()>
;; scheme@(guile-user)> ((q 'empty?))
;; $8 = #t
;; scheme@(guile-user)> ((q 'insert!) 1)
;; scheme@(guile-user)> ((q 'print))
;; 1 
;; scheme@(guile-user)> ((q 'insert!) 2)
;; scheme@(guile-user)> ((q 'insert!) 3)
;; scheme@(guile-user)> ((q 'delete!))
;; scheme@(guile-user)> ((q 'insert!) 4)
;; scheme@(guile-user)> ((q 'print))
;; 2 3 4 

