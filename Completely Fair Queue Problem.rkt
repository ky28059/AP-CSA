; Below is an abstraction for queues, a make-teacher procedure, and a pay-cashier procedure. 
; In the problem, we will simulate a shopping queue, where people line up and pay a cashier.
; But note that this queue is quite unfair. For most teachers at the front of the queue,
; they can pay and exit the queue as normal. However, calculus teachers at the front of the queue are
; penalized and are thus sent to the back of the queue. This is because they are horrible people.
; 
; The second time a calculus teacher reaches the front of the queue,
; they are are allowed to pay and exit the queue. 

; Complete the pay-cashier procedure to make this program run. DO NOT change anything in the
; make-queue code. 
; 
; ;Sample code:
; (define deggeller (make-teacher 'deggeller 'calc))
; (define bautista (make-teacher 'bautista 'calc))
; (define paley (make-teacher 'paley 'cs))
; (define hexsel (make-teacher 'hexsel 'cs))
; 
; (define shopping-q (make-queue))
; 
; ((shopping-q 'insert-queue!) deggeller)
; ((shopping-q 'insert-queue!) paley)
; ((shopping-q 'insert-queue!) bautista)
; ((shopping-q 'insert-queue!) hexsel)
; (pay-cashier! shopping-q)
; (peek-at shopping-q)
; (pay-cashier! shopping-q)
; (peek-at shopping-q)
; (pay-cashier! shopping-q)
; (peek-at shopping-q)


; Output from above code if things are working properly:
; (deggeller has gone to the back of the line)
; paley
; (paley has paid)
; bautista
; (bautista has gone to the back of the line)
; hexsel




; A QUEUE ABSTRACTION - DON'T CHANGE THIS!
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; empty-queue? is written to align with the way front-ptr
    ; and rear-ptr were given, above
    (define (empty-queue?)
      ;(and (null? front-ptr) (null? rear-ptr)))
      (null? front-ptr))
    (define (set-front-ptr! pair)
      (set! front-ptr pair))
    (define (set-rear-ptr! pair)
      (set! rear-ptr pair))

    ; insert! plays out differently depending on whether the queue
    ; is currently empty or not
    (define (insert! item)
      (let ((new-pair (list item)))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else  
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))

    ; delete! has three possibilties:
    ; * empty queue
    ; * one element in queue
    ; * more than one element in queue
    (define (delete!)
      (cond ((empty-queue?) "Nothing to delete! Empty queue!")
             ; store the datum at the head of the queue
            (else (let ((return-value (peek)))
                     ; update the front pointer
                    (set-front-ptr! (cdr front-ptr))
                    ; If there was only one thing in the queue, then the
                    ; rear-ptr will need to be set to nil
                    (cond ((null? front-ptr) (set-rear-ptr! '())))
                    return-value)))) ; Now return the element of the queue (or #f)

    ; peek returns the datum at the front of the queue
    ; peek returns #f if the queue is empty
    (define (peek)
      (if (empty-queue?)
          (display "Empty queue!")
          (car front-ptr)))
    (define (show)
      (show-helper front-ptr))
    (define (show-helper lyst)
      (cond ((null? lyst) '())
            (else (cons ((car lyst) 'name) (show-helper (cdr lyst))))))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert!)
            ((eq? m 'delete-queue!) delete!)
            ((eq? m 'empty?) empty-queue?)
            ((eq? m 'peek) peek) 
            ((eq? m 'show) show))) ; shows a list of who's in the queue
    dispatch))

;THE MAKE-TEACHER PROCEDURE - DON'T CHANGE THIS!
(define (make-teacher name subject)
  (let ((penalized #f))
    (define (discriminate) (set! penalized #t))
    (define (get-out-of-jail) (set! penalized #f))
    ; oktopay?: if the teacher is not a calc teacher or
    ; a teacher that has already been penalized
    (define (oktopay?) (or (not (eq? subject 'calc)) penalized))
    (define (get-name) name)
    (lambda (msg)
      (cond ((eq? msg 'penalize-calc) discriminate)
            ((eq? msg 'get-out-of-jail) get-out-of-jail)
            ((eq? msg 'name) get-name)
            ((eq? msg 'oktopay?) oktopay?)))))

;COMPLETE THIS PROCEDURE
(define (pay-cashier! shopping-queue)
  (let ((payer ((shopping-queue 'peek)))) ; who is in the front of the queue?
    (cond (((payer 'oktopay?)) ; will we let them pay?
           ((payer 'get-out-of-jail)) ; calc teacher no longer penalized
           (display (cons ((((shopping-queue 'delete-queue!)) 'name)) '(has paid)))
           (newline))
          (else ; a calc teacher who needs to be penalized!
           ((payer 'penalize-calc)) ; penalize person for being a calc teacher
           (display (cons ((payer 'name)) '(has gone to the back of the line)))
           (newline)
           ; Actually put the calc teacher in the back of the queue
           ((shopping-queue 'insert-queue!) ((shopping-queue 'delete-queue!)))))))

(define (peek-at shopping-queue)
  ((((shopping-queue 'peek)) 'name)))

;CODE FOR YOU TO USE TO TEST YOUR PROGRAM (same as in comments)
(define deggeller (make-teacher 'deggeller 'calc))
(define bautista (make-teacher 'bautista 'calc))
(define paley (make-teacher 'paley 'cs))
(define hexsel (make-teacher 'hexsel 'cs))
(define shopping-q (make-queue))

((shopping-q 'insert-queue!) deggeller)
((shopping-q 'insert-queue!) paley)
((shopping-q 'insert-queue!) bautista)
((shopping-q 'insert-queue!) hexsel)
(peek-at shopping-q)
(pay-cashier! shopping-q)
(peek-at shopping-q)
(pay-cashier! shopping-q)
(peek-at shopping-q)
(pay-cashier! shopping-q)
(peek-at shopping-q)
(pay-cashier! shopping-q)
(peek-at shopping-q)
(pay-cashier! shopping-q)
(peek-at shopping-q)
(pay-cashier! shopping-q)