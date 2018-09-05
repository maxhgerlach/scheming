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
      (cons (car x) (append (cdr x) y))))

(define (my-append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (my-last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (my-append x y))

;; z                                       ; (a b c d)

;; (cdr x)                                 ; (b)

(define w (my-append! x y))

;; w                                       ; (a b c d)

;; (cdr x)                                 ; (b c d)
