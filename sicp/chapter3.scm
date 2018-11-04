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

;(set-value! F 212 'user)
;; ERROR: In procedure scm-error:
;; Contradiction (77 212)

(forget-value! C 'user)
;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?

(set-value! F 212 'user)
;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100


;; Exercise 3.33

(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (constant 0.5 y)
    (multiplier x y c)
    'ok))

(define num1 (make-connector))
(define num2 (make-connector))
(define avg (make-connector))

(averager num1 num2 avg)

(probe "num1" num1)
(probe "num2" num2)
(probe "avg" avg)

(set-value! num1 14 'user)
;; Probe: num1 = 14
(set-value! num2 20 'user)
;; Probe: num2 = 20
;; Probe: avg = 17.0
(forget-value! num1 'user)
;; Probe: num1 = ?
;; Probe: avg = ?
(forget-value! num2 'some-other-user)
;; 'ignored
(set-value! avg 100 'user)
;; Probe: avg = 100
;; Probe: num1 = 180.0


;; Ex. 3.34
;;
;; Louis Reasoner's squarer
;;
;; Setting a to get b works.  But setting b to get a does not work,
;; because the following condition in multiplier is never met:
;;
;; ((and (has-value? product) (has-value? m1))
;;  (set-value! m2
;;              (/ (get-value product)
;;                 (get-value m1))
;;              me))


(define (louis-squarer a b)
  (multiplier a a b))

(define louis-a (make-connector))
(define louis-b (make-connector))

(louis-squarer louis-a louis-b)

(probe "a" louis-a)
(probe "b" louis-b)

(set-value! louis-a 9 'user)
;; Probe: a = 9
;; Probe: b = 81

(forget-value! louis-a 'user)
;; Probe: a = ?
;; Probe: b = ?

(set-value! louis-b 99 'user)
;; Probe: b = 99


;; Ex. 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a) (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define proper-a (make-connector))
(define proper-b (make-connector))

(squarer proper-a proper-b)

(probe "a" proper-a)
(probe "b" proper-b)

(set-value! proper-a 9 'user)
;; Probe: a = 9
;; Probe: b = 81

(forget-value! proper-a 'user)
;; Probe: a = ?
;; Probe: b = ?

(set-value! proper-b 99 'user)
;; Probe: b = 99
;; Probe: a = 9.9498743710662



;; Ex. 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv x)
  (let ((x-connector (make-connector)))
    (constant x x-connector)
    x-connector))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius" C)
(probe "Fahrenheit" F)

(set-value! C 100 'user)
;; Probe: Celsius = 100
;; Probe: Fahrenheit = 212

(forget-value! C 'user)
;; Probe: Celsius = ?
;; Probe: Fahrenheit = ?

(forget-value! F 'user)
(set-value! F 72.0 'user)
;; Probe: Fahrenheit = 72.0
;; Probe: Celsius = 22.22222222222222



;; Exercise 3.47 Semaphore
(define false #f)

(define true #t)

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)            ; non-atomic
  (if (car cell) true (begin (set-car! cell true) false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))


;; (define (list-from-constructor n constructor)
;;   (if (> n 0)
;;       (cons (constructor) (list-from-constructor (- n 1) constructor))
;;       '()))


(define (make-semaphore_mutex n)
  (let ((my-mutex (make-mutex))
        (counter 0))
    (define (acquire)
      (my-mutex 'acquire)
      (cond ((< counter n)
             (set! counter (+ counter 1))
             (my-mutex 'release))
            (else
             (my-mutex 'release)
             (acquire))))   ; retry
    (define (release)
      (my-mutex 'acquire)
      (if (> counter 0)
          (set! counter (- counter 1)))
      (my-mutex 'release))          
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "Unknown message sent to semaphore" m))))
    the-semaphore))
    
  
(define (make-semaphore_test-and-set n)
  (let ((cell (list false))
        (counter 0))
    (define (acquire)
      (if (test-and-set! cell)
          (acquire)                     ; retry
          (cond ((< counter n)
                 (set! counter (+ counter 1))
                 (clear! cell))
                (else
                 (clear! cell)
                 (acquire)))))   ; retry
    (define (release)
      (if (test-and-set! cell)
          (release)                     ; retry
          (begin
            (if (> counter 0)
                (set! counter (- counter 1)))
            (clear! cell))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "Unknown message sent to semaphore" m))))
    the-semaphore))


;; Ex. 3.48 | Deadlock avoidance

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define last-id 0)

(define (make-id)
  (set! last-id (+ last-id 1))
  last-id)


(define (make-account-and-serializer balance)
  (let ((account-id (make-id)))
    (define (get-id) account-id)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'account-id) account-id)
              ((eq? m 'serializer) balance-serializer)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch)))


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


(define (serialized-exchange account1 account2)
  (let ((id-1 (account1 'account-id))
        (id-2 (account2 'account-id)))
    (let ((first-account (cond ((< id-1 id-2)
                                account1)
                               ((> id-1 id-2)
                                account2)
                               (else
                                (error "equal ids!"))))
          (second-account (cond ((< id-1 id-2)
                                account2)
                               ((> id-1 id-2)
                                account1)
                               (else
                                (error "equal ids!")))))
      (let ((serializer1 (first-account 'serializer))
            (serializer2 (second-account 'serializer)))
        ((serializer1 (serializer2 exchange))
         account1
         account2)))))


;; 3.5 Streams

;; 3.5.1 Streams as Delayed Lists

;; (delay <exp>) could have been syntactic sugar for (lambda ()
;; <exp>), but really would include memoization

;; force may be implemented as a procedure (define (force
;; delayed-object) (delayed-object))

;; (cons-stream <a> <b>) <=> (cons <a> (delay <b>))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (display x) (newline))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; (define (square x) (* x x))
;; (define (smallest-divisor n) (find-divisor n 2))
;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (else (find-divisor n (+ test-divisor 1)))))
;; (define (divides? a b) (= (remainder b a) 0))

;; (define (prime? n)
;;   (= n (smallest-divisor n)))

;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime?
;;                  (stream-enumerate-interval
;;                   10000 1000000))))     ; 10009


;; Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map
                    stream-car argstreams))
       (apply stream-map
              (cons proc (map
                          stream-cdr argstreams))))))

;; (display-stream (stream-map +
;;                             (stream-enumerate-interval 1 10)
;;                             (stream-enumerate-interval 200 209)))
;; 201
;; 203
;; 205
;; 207
;; 209
;; 211
;; 213
;; 215
;; 217
;; 219


;; Exercise 3.51

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
;; 0

(stream-ref x 5)

;; => 5
;;
;; 1
;; 2
;; 3
;; 4
;; 5

;; [due to delay memoization, each stream element is only shown the
;; first time it is accessed]

(stream-ref x 7)

;; => 7

;; 6
;; 7



;; Exercise 3.52

(define sum 0)                          ; 0
(define (accum x) (set! sum (+ x sum)) sum) ; 0
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20))) ; 1
;; seq is the stream of 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136... 
(define y (stream-filter even? seq))             ; 6
;; elements this looks at via stream-car: 1, 3, 6 (first even)
;; y is the stream of 6, 10, 28, 36, 66, 78, 120, 136, ...

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))                  ; 10
;; elements this looks at via stream-car: 1, 3, 6, 10 (first with remainder 0)
(stream-ref y 7)                        ; => 136, sum=136

(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210,     sum = 210


;; Without memoization the side effect would be triggered anew every
;; time the stream seq is iterated over.  This would surely change
;; results.


;; Sec. 3.5.2 Infinite Streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; (stream-ref no-sevens 100)              ; 117

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;; Sieve of Eratosthenes
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;; (stream-ref primes 0)                   ;2
;; (stream-ref primes 1)                   ;3
;; (stream-ref primes 50)                  ;233
;; (stream-ref primes 500)                 ;3581
;; (stream-ref primes 2000)                ;17393
;; (stream-ref primes 4000)                ;37831
;; (stream-ref primes 5000)                ;48619
;; (stream-ref primes 5050)                ;49139


;; Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;; Ex. 3.53

(define s (cons-stream 1 (add-streams s s)))
;; 1 2 4 8 16 32 ...

(define (display-n n s)
  (display-stream (stream-map (lambda (idx) (stream-ref s idx))
                              (stream-enumerate-interval 0 n))))
(define (display10 s) (display-n 10 s))

;; 1
;; 2
;; 4
;; 8
;; 16
;; 32
;; 64
;; 128
;; 256
;; 512
;; 1024


;; Ex. 3.54

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

(display10 factorials)

;; 1
;; 1
;; 2
;; 6
;; 24
;; 120
;; 720
;; 5040
;; 40320
;; 362880
;; 3628800


;; Ex. 3.55

;; (define (partial-sums s)
;;   (cons-stream (stream-car s) (stream-map
;;                                (lambda (val) (+ (stream-car s) val))
;;                                (partial-sums (stream-cdr s)))))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(display10 (partial-sums integers))

;; 1
;; 3
;; 6
;; 10
;; 15
;; 21
;; 28
;; 36
;; 45
;; 55
;; 66


;; Ex. 3.56

;; all integers that can be built from products of factors 2, 3, or 5

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge
                          (merge (scale-stream S 2)
                                 (scale-stream S 3))
                          (scale-stream S 5))))

(display-n 30 S)


;; Ex. 3.57
;;
;; fibs implemented via add-streams with memoization in delay (see
;; above): No addition needed to recall previous Fibonacci numbers ->
;; to get the n-th Fibonacci number we need n-1 additions.
;;
;; Without memoization all terms need to be recomputed:
;;  additions(n) = additions(n-1) + additions(n-2)
;; This is the Fibonacci relation, solution: additions(n) = \phi ^ n


;; Ex. 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; stream of digits represting the fraction (num/den) in base radix

; (display-n 10 (expand 1 7 10))
; => 1 4 2 8 5 7 1 4 2 8 5 ...

; (display-n 10 (expand 3 8 10))
; => 3 7 5 0 0 0 0 0 0 0 0 ...


;; Ex. 3.59

;; a

(define (integrate-series coeff-stream)
  (define (integrate-terms idx coeffs)
    (cons-stream
     (/ (stream-car coeffs) idx)
     (integrate-terms (+ 1 idx) (stream-cdr coeffs))))
  (integrate-terms 1 coeff-stream))

;; (display-n 4 (integrate-series (stream-enumerate-interval 30.0 40.0)))


;; b

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;(display-n 10 exp-series)               ; 1 + x + 1/2 x^2 + 1/6 x^3 + ...

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(display-n 10 cosine-series)            ; 1 - 1/2 x^2 + 1/24 x^4 + ... 
(display-n 10 sine-series)              ; x - 1/6 x^3 + 1/120 x^5 + ...


;; Ex. 3.60

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

;; (display-n 10 (add-series
;;                (mul-series sine-series sine-series)
;;                (mul-series cosine-series cosine-series)))


;; Ex. 3.61

(define (invert-unit-series s)
  (cons-stream
   1
   (mul-series (scale-stream (stream-cdr s) -1) (invert-unit-series s))))

;; (display-n 10 (invert-unit-series exp-series))


;; Ex. 3.62

(define (div-series s1 s2)
  (let ((a (stream-car s2)))
    (if (= a 0)
        (error "The constant term of s2 may not be zero"))
    (let ((ai (/ 1 a)))
      (mul-series
       (scale-stream s1 ai)
       (invert-unit-series (scale-stream s2 ai))))))

(define tangent-series (div-series sine-series cosine-series))

;; (display-n 10 tangent-series)



;; Formulating iterations as stream processes

(define (average x y)
  (/ (+ x y) 2))
     
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (square a)
  (* a a))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

;; (display-n 10 (sqrt-stream 2))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
                                        ; S n −1
        (s1 (stream-ref s 1))
                                        ; S n
        (s2 (stream-ref s 2)))
                                        ; S n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


;; (display-n 10 (euler-transform (sqrt-stream 2)))  ;; nan's quickly

;; (display-n 10 (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; (display-n 10
;;  (accelerated-sequence euler-transform pi-stream))


;; Exercise 3.63
;;
;; Less efficient:
;; 
;; (define (sqrt-stream x)
;;   (cons-stream 1.0 (stream-map
;;                     (lambda (guess)
;;                       (sqrt-improve guess x))
;;                     (sqrt-stream x))))
;;
;; This will build new cons-stream's again and again, thus voiding the
;; benefits of memoization. [I probably made the same mistake above in
;; div-series etc.]. Without memoization in delay both versions would
;; be equally inefficient.


;; Exercise 3.64

(define (stream-limit s tolerance)
  (define (continue prev s-cdr)
    (cond ((stream-null? s-cdr)
           prev)
          ((<= (abs (- prev (stream-car s-cdr))) tolerance)
           (stream-car s-cdr))
          (else
           (continue (stream-car s-cdr) (stream-cdr s-cdr)))))
  (continue (stream-car s) (stream-cdr s)))

(define (tol-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; (tol-sqrt 2.0 0.00000000000001)         ;=> 1.414213562373095
;; (tol-sqrt 2.0 0.1)                      ;=> 1.4166666666666665
;; (tol-sqrt 2.0 0.01)                     ;=> 1.4142156862745097


;; Exercise 3.65
;;
;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...   ~ 0.69314718056

(define (iterations-until-tolerance s tolerance)
  (define (continue prev iterations s-cdr)
    (cond ((stream-null? s-cdr)
           '())
          ((<= (abs (- prev (stream-car s-cdr))) tolerance)
           iterations)
          (else
           (continue (stream-car s-cdr) (+ 1 iterations) (stream-cdr s-cdr)))))
  (continue (stream-car s) 1 (stream-cdr s)))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; (stream-limit ln2-stream 0.001)          ;=> 0.6936464315588232
;; (iterations-until-tolerance ln2-stream 0.001) ;=> 1000

;; (stream-limit (euler-transform ln2-stream) 0.001) ;=>0.6928571428571428
;; (iterations-until-tolerance (euler-transform ln2-stream) 0.001) ;=> 5
;; (stream-limit (euler-transform ln2-stream) 0.000001) ;=> 0.6931466925212173
;; (iterations-until-tolerance (euler-transform ln2-stream) 0.000001) ;=> 61

;; (stream-limit (accelerated-sequence euler-transform ln2-stream) 0.001) ;=> 0.6931488693329254
;; (iterations-until-tolerance (accelerated-sequence euler-transform ln2-stream) 0.001) ;=> 3
;; (stream-limit (accelerated-sequence euler-transform ln2-stream) 0.000001) ;=> 0.6931471806635636
;; (iterations-until-tolerance (accelerated-sequence euler-transform ln2-stream) 0.000001) ;=> 5


;; Infinite streams of pairs

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; Ex. 3.66
(define int-pairs (pairs integers integers))

;; (display-n 20 int-pairs)

;; 1(1 1) 2(1 2) 4(1 3)  6(1 4)  8(1 5) 10(1 6) 12(1 7) 14(1 8) 16(1 9) 18(1 10) 20(1 11)
;;        3(2 2) 5(2 3)  9(2 4) 13(2 5) 17(2 6) 21(2 7)
;;               7(3 3) 11(3 4) 19(3 5)
;;                      15(4 4)


(define (stream-find s val max-iter)
  (define (iter sub-s i)
    (cond ((equal? (stream-car sub-s) val)
           i)
          ((>= i max-iter)
           '())
          (else (iter (stream-cdr sub-s) (+ 1 i)))))
  (iter s 0))

;; (stream-find int-pairs (list 1 1) 10000)   ; 0
;; (stream-find int-pairs (list 1 100) 10000) ; 197 [this is the number of pairs preceding (1 100)]
;; (stream-find int-pairs (list 2 100) 10000) ; 392 (2 * 197 - 2)
;; (stream-find int-pairs (list 3 100) 10000) ; 778
;; (stream-find int-pairs (list 4 100) 100000) ; 1542
;; (stream-find int-pairs (list 99 100) 1000000) ; '() ...  should be on the order of 2^99 * 100 (a bit less)



;; Ex. 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

;; (stream-ref (all-pairs integers integers) 0) ;(1 1)
;; (stream-ref (all-pairs integers integers) 1) ;(1 2)

;; (display-n 20 (all-pairs integers integers))
;;  0(1 1)  1(1 2)  5(1 3)  9(1 4) 13(1 5) 17(1 6)
;;  3(2 1)  2(2 2)  4(2 3) 12(2 4) 20(2 5)
;;  7(3 1)  8(3 2)  6(3 3) 10(3 4)
;; 11(4 1) 16(4 2) 18(4 3) 14(4 4)
;; 15(5 1)
;; 19(6 1)
;; 

;; The order is a bit strange (gaps are filled up rather late, but it works)



;; Ex. 3.68

;; (define (louis-pairs s t)
;;   (interleave
;;    (stream-map (lambda (x) (list (stream-car s) x))
;;                t)
;;    (louis-pairs (stream-cdr s) (stream-cdr t))))

;; This will recur infinitely. Both arguments to interleave are
;; evaluated eagerly (it's a procedure, not a macro). But louis-pairs
;; called as an argument to interleave never produces a non-delayed
;; stream element!


;; Ex. 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs t u))            ; probably not very efficient
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

;; (display-n 10 (triples integers integers integers))
;; =>
;; (1 1 1)
;; (1 1 1)
;; (2 2 2)
;; (1 1 2)
;; (2 2 2)
;; (1 2 2)
;; (3 3 3)
;; (1 1 3)
;; (2 2 3)
;; (1 2 3)
;; (3 3 3)

(define pythagorean-triples
  (stream-filter (lambda (triple) (let ((i (car triple))
                                        (j (cadr triple))
                                        (k (caddr triple)))
                                    (= (+ (square i) (square j))
                                       (square k)))) ; i <= j automatically
                 (triples integers integers integers)))

;; (stream-car pythagorean-triples)        ; (3 4 5)
;; (stream-ref pythagorean-triples 1)      ; (6 8 10)
;; (stream-ref pythagorean-triples 2)      ; (5 12 13)


;; Exercise 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight)))
                 (else                  ;include both
                  (cons-stream
                   s1car
                   (cons-stream
                    s2car
                    (merge-weighted (stream-cdr s1)
                                    (stream-cdr s2)
                                    weight)))))))))

(define (weighted-pairs s t pair-weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
    pair-weight)))

;; a
(define int-pairs-weighted
  (weighted-pairs integers integers
                  (lambda (pair) (apply + pair))))

;; (display-n 40 int-pairs-weighted)
