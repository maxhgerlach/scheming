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


;; Ex. 3.3
(define (make-account balance stored-password)
  (define (checked password fn)
    (if (eq? password stored-password)
        (lambda (amount) (fn amount))
        (lambda (_) "Incorrect password")))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (cond ((eq? m 'withdraw) (checked password withdraw))
          ((eq? m 'deposit) (checked password deposit))
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;; scheme@(guile-user)> (define A (make-account 100 'secret-password))
;; scheme@(guile-user)> ((A 'wrong-password 'withdraw) 10)
;; $21 = "Incorrect password"
;; scheme@(guile-user)> ((A 'secret-password 'withdraw) 10)
;; $22 = 90
;; scheme@(guile-user)> ((A 'wrong-password 'deposit) 20)
;; $23 = "Incorrect password"
;; scheme@(guile-user)> ((A 'secret-password 'deposit) 20)
;; $24 = 110

