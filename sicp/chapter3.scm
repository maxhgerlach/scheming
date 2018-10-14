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

(define (make-queue) (cons '() '()))

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

(define (ex-make-queue)
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

;; scheme@(guile-user)> (define q (ex-make-queue))
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


;; Exercise 3.23

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (or (null? front-ptr) (null? rear-ptr)))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty deque")
          (car front-ptr)))
    (define (rear)
      (if (empty?)
          (error "REAR called with an empty deque")
          (car rear-ptr)))
    (define (next-node node)
      (car (cdr node)))
    (define (prev-node node)
      (car (cddr node)))
    (define (set-next-node! node target)
      (set-car! (cdr node) target))
    (define (set-prev-node! node target)
      (set-car! (cddr node) target))
    (define (rear-insert! item)
      (let ((new-node (list item '() rear-ptr)))
        (cond ((empty?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-next-node! rear-ptr new-node)
               (set! rear-ptr new-node)))))
    (define (front-delete!)
      (cond ((empty?)
             (error "FRONT-DELETE! called with an empty queue"))
            (else
             (set! front-ptr (next-node front-ptr))
             (if (null? front-ptr)
                 (set! rear-ptr '())
                 (set-prev-node! front-ptr '())))))
    (define (front-insert! item)
      (let ((new-node (list item front-ptr '())))
        (cond ((empty?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-prev-node! front-ptr new-node)
               (set! front-ptr new-node)))))
    (define (rear-delete!)
      (cond ((empty?)
             (error "REAR-DELETE! called with an empty queue"))
            (else
             (set! rear-ptr (prev-node rear-ptr))
             (if (null? rear-ptr)
                 (set! front-ptr '())
                 (set-next-node! rear-ptr '())))))
    (define (print)
      (define (iter ptr)
        (cond ((null? ptr) '())
              (else
               (display (car ptr))
               (display " ")
               (iter (next-node ptr)))))
      (iter front-ptr)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'front) (front))
            ((eq? m 'rear) (rear))
            ((eq? m 'front-insert!) front-insert!)
            ((eq? m 'rear-insert!) rear-insert!)
            ((eq? m 'front-delete!) (front-delete!))
            ((eq? m 'rear-delete!) (rear-delete!))
            ((eq? m 'print) (print))
            (else (error "DEQUE does not know " m))))
    dispatch))


;; 3.3.3 Representing Tables

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define example (make-table))
((example 'insert-proc!) 'math '+ 43)
((example 'insert-proc!) 'math '- 45)
((example 'insert-proc!) 'math '* 42)
((example 'insert-proc!) 'letters 'a 97)
((example 'insert-proc!) 'letters 'b 98)

((example 'lookup-proc) 'math '/) ;#f
((example 'lookup-proc) 'math '-) ;45
((example 'lookup-proc) 'letters 'b)    ;98


;; Exercise 3.24

(define (make-table-test same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define example2 (make-table-test (lambda (v1 v2) (< (abs (- v1 v2)) 0.1))))
((example2 'insert-proc!) 0.1 9.9 10.0)
((example2 'lookup-proc)  0.1 9.9)      ; 10.0
((example2 'lookup-proc)  0.05 1.0)     ; #f
((example2 'lookup-proc)  0.05 9.99999) ; 10.0


;; Exercise 3.25

(define (make-table-multi-keys)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (sub-lookup key-list table)
        (let ((subtable-or-record
               (assoc (car key-list) (cdr table))))
          (if subtable-or-record
              (if (null? (cdr key-list))
                  (cdr subtable-or-record) ; return record value
                  (sub-lookup (cdr key-list) subtable-or-record))
              #f)))
      (sub-lookup key-list local-table))
    (define (at-least-two-elements? lst)
      (cond ((null? lst) #f)
            ((null? (cdr lst)) #f)
            (else #t)))
    (define (insert! key-list value)
      (define (sub-insert! key-list value table)
        (if (at-least-two-elements? key-list)
            ;; first key specifies subtable
            (let ((subtable
                   (assoc (car key-list) (cdr table)))
                  (remaining-keys
                   (cdr key-list)))
              (if subtable
                  ;; insert into subtable created before
                  (sub-insert! remaining-keys value subtable)
                  ;; create new subtable, then insert
                  (begin
                    (set! subtable (list (car key-list)))
                    (set-cdr! table (cons subtable
                                          (cdr table)))
                    (sub-insert! remaining-keys value subtable))))
            ;; first (and only remaining) key specifies record
            (let ((record
                   (assoc (car key-list) (cdr table))))
              (if record
                  ;; modify record that's already present
                  (set-cdr! record value)
                  ;; insert new record
                  (set-cdr! table
                            (cons (cons (car key-list)
                                        value)
                                  (cdr table))))))
        'ok)
      (sub-insert! key-list value local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;; scheme@(guile-user)> (define ex (make-table-multi-keys))
;; scheme@(guile-user)> ((ex 'insert-proc!) '(math +) 43)
;; $2 = ok
;; scheme@(guile-user)> ((ex 'insert-proc!) '(math -) 45)
;; $3 = ok
;; scheme@(guile-user)> ((ex 'insert-proc!) '(math -) 45)
;; $4 = ok
;; scheme@(guile-user)> ((ex 'insert-proc!) '(math -) 455)
;; $5 = ok
;; scheme@(guile-user)> ((ex 'lookup-proc) '(math +))
;; $6 = 43
;; scheme@(guile-user)> ((ex 'lookup-proc) '(math -))
;; $7 = 455
;; scheme@(guile-user)> ((ex 'insert-proc!) '(stuff letters a) 999)
;; $8 = ok
;; scheme@(guile-user)> ((ex 'lookup-proc) '(math -))
;; $9 = 455
;; scheme@(guile-user)> ((ex 'lookup-proc) '(stuff -))
;; $10 = #f
;; scheme@(guile-user)> ((ex 'lookup-proc) '(stuff letters))
;; $11 = ((a . 999))
;; scheme@(guile-user)> ((ex 'lookup-proc) '(stuff letters a))
;; $12 = 999



;; Section 3.3.4 A Simulator for Digital Circuits

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((= s1 0)
         (cond ((= s2 0) 0)
               ((= s2 1) 0)
               (else "Invalid signal s2: " s2)))
        ((= s1 1)
         (cond ((= s2 0) 0)
               ((= s2 1) 1)
               (else "Invalid signal s2: " s2)))
        (else (error "Invalid signal s1: " s1))))


;; Exercise 3.28

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((= s1 0)
         (cond ((= s2 0) 0)
               ((= s2 1) 1)
               (else "Invalid signal s2: " s2)))
        ((= s1 1)
         (cond ((= s2 0) 1)
               ((= s2 1) 1)
               (else "Invalid signal s2: " s2)))
        (else (error "Invalid signal s1: " s1))))


;; Exercise 3.29

(define (compound-or-gate o1 o2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (a (make-wire)))
    (inverter o1 n1)
    (inverter o2 n2)
    (and-gate n1 n2 a)
    (inverter a output)))

; delay time: inverter-delay + and-delay + inverter-delay


;; Exercise 3.30

(define (ripple-carry-adder a_k b_k s_k c)
  (if (not (= (length a_k) (length b_k) (length s_k)))
      (error "mismatching wire numbers"))
  (let ((null-c-in (make-wire)))
    (define (apply-full-adders my-a_k my-b_k c-in my-s_k final-c-out)
      (cond ((null? my-a_k)
             (set-signal! final-c-out (get-signal c-in))
             'ok)
            (else
             (let ((c-out (make-wire)))
               (full-adder (car my-a_k) (car my-b_k) c-in (car my-s_k) c-out)
               (apply-full-adders (cdr my-a_k) (cdr my-b_k) c-out (cdr my-s_k) final-c-out)
               ))))
    (set-signal! null-c-in 0)
    (apply-full-adders a_k b_k null-c-in s_k c)))

;; n-bit ripple-carry-adder delay (carry propagation)
;;  = n * full-adder-carrydelay
;;  = n * (2 * half-adder-carry-delay + or-delay)
;;  = n * (2 * and-delay + or-delay)

;; delay on s is larger (on half-adder level)


;; Representing wires, 

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))


(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))


(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


;; A sample simulation

(define (wire-probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; scheme@(guile-user)> (wire-probe 'sum sum)

;; sum 0 New-value = 0
;; scheme@(guile-user)> (wire-probe 'carry carry)

;; carry 0 New-value = 0
;; scheme@(guile-user)> (half-adder input-1 input-2 sum carry)
;; $2 = ok
;; scheme@(guile-user)> (set-signal! input-1 1)
;; $3 = done
;; scheme@(guile-user)> (propagate)

;; sum 8 New-value = 1
;; $4 = done
;; scheme@(guile-user)> (set-signal! input-2 1)
;; $5 = done
;; scheme@(guile-user)> (propagate)

;; carry 11 New-value = 1

;; sum 16 New-value = 0
;; $6 = done
;; scheme@(guile-user)> 



;; Sec. 3.3.5 Propagation of Constraints

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)


(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)


(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))


(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))


(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; Constraint system for Celsius <-> Fahrenheit

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77

(set-value! F 212 'user)
;; ERROR: In procedure scm-error:
;; Contradiction (77 212)

(forget-value! C 'user)
;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?

(set-value! F 212 'user)
;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100
