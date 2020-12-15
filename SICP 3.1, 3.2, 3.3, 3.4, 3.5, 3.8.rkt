; SICP 3.1, define make-accumulator
(define (make-accumulator sum)
  (lambda (num)
    (set! sum (+ sum num))
    sum))


; SICP 3.2, define make-monitored
(define (make-monitored f)
  (set! count 0)
  (define (dispatch msg)
    (cond ((eq? msg 'how-many-calls?) count)
          ((eq? msg 'reset-count) (set! count 0))
          (else (set! count (+ 1 count))
                (f msg))))
  dispatch)

; SICP 3.3, refactor make-account to use password
(define (make-account balance password)
  (define (reject-password x) "Incorrect password")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        reject-password))
  dispatch)

; SICP 3.4, if password is rejected 7 consecutive times call the cops
(define (make-account balance password)
  (let ((pw-tries 0))
    (define (reject-password x) 
      (set! pw-tries (+ pw-tries 1))
      (if (> pw-tries 6) 
          (call-the-cops x)
          "Incorrect password"))
    (define (call-the-cops x) 'WEEWOOWEEWOO)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (if (eq? pass password)
          (begin (set! pw-tries 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    m))))
          reject-password))
    dispatch))

; SICP 3.5, joint-account
(define (make-password-counter password)
  (let ((pw-tries 0))
    (define (reject-password x) 
      (set! pw-tries (+ pw-tries 1))
      (if (> pw-tries 6) 
          (lambda (x) (call-the-cops x))
          (lambda (x) "Incorrect password")))
    (define (call-the-cops x) 'WEEWOOWEEWOO)
    (define (dispatch pass)
      (if (eq? pass password) #t
          (reject-password pass)))
    dispatch))

(define (make-account balance password)
  (let ((pass-counter (make-password-counter password)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (let ((check (pass-counter pass))) ; To prevent double calls
        (if (equal? check #t) ; If the password check returned true
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'password) pass-counter) ; Returns the password counter representing the account password
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m)))
            check))) ; Otherwise, return the "wrong password" message
    dispatch))

(define (make-joint account existingpass newpass)
  (let ((pass-counter (make-password-counter newpass)))
    (define (dispatch pass m)
      (let ((check (pass-counter pass))) ; To prevent double calls
        (if (equal? check  #t) ; If the password check returned true
            (account existingpass m)
            check))) ; Otherwise, return the "wrong password" message
    (if (equal? ((account existingpass 'password) existingpass) #t) ; If password check of existingpass on the password counter from account returns true
        dispatch
        ((account existingpass 'password) existingpass)))) ; Otherwise, return the "wrong password" message

; SICP 3.8, define f
(define f
  (let ((num-uses 0))
    (define (dispatch num)
      (cond {(= num-uses 0)
             (set! num-uses (+ 1 num-uses))
             num}
            {else 0}))
    dispatch))

