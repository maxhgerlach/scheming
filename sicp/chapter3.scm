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
