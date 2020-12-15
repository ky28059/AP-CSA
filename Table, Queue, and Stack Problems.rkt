; Table problems
(define (make-table)
  (cons '* '()))
(define (empty-table? t) (null? (cdr t)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
        
(define (lookup k t)
  (let ((record (assoc k (cdr t))))
    (cond (record (cdr record))
          (else #f))))
(define (assocc key records) ; Renamed to assocc because R5RS does not let you redefine primitives
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assocc key (cdr records)))))

(define (rlookup k t)
  (let ((record (rassoc k (cdr t))))
    (cond (record (car record))
          (else #f))))
(define (rassoc value records)
  (cond ((null? records) #f)
        ((equal? value (cdar records)) (car records))
        (else (rassoc value (cdr records)))))

(define (delete! k t)
  (let ((to-delete-pair (assocc k (cdr t))))
    (cond ((not to-delete-pair) #f)
          ((eq? (cadr t) to-delete-pair) (set-cdr! t (cddr t)))
          (else (delete! k (cdr t))))))

; Queue problems
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; empty-queue? is written to align with the way front-ptr
    ; and rear-ptr were given, above
    (define (empty-queue?)
      (eq? front-ptr '()))

    ; peek returns the datum at the front of the queue
    ; peek returns #f if the queue is empty
    (define (peek)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else (car front-ptr))))

    ; insert-queue! plays out differently depending on whether the queue
    ; is currently empty or not
    (define (insert-queue! datum)
      (let ((new-node (cons datum '())))
        (if (empty-queue?)
            (begin
              (set! front-ptr new-node)
              (set! rear-ptr new-node))
            (begin
              (set-cdr! rear-ptr new-node)
              (set! rear-ptr (cdr rear-ptr))))))

    ; delete-queue! has three possibilties:
    ; * empty queue
    ; * one element in queue
    ; * more than one element in queue
    (define (delete-queue!)
      (cond ((empty-queue?) (error "Empty queue.  :-("))
            (else 
                    ; store the datum at the head of the queue
                  (let ((return-value (peek)))
                    ; update the front pointer
                    (set! front-ptr (cdr front-ptr))
                    ; If there was only one thing in the queue, then the
                    ; rear-ptr will need to be set to nil
                    (if (null? front-ptr) (set! rear-ptr '()))
                    ; Now return the element of the queue (or #f)
                    return-value))))

    (define (dispatch message)
      (cond ((eq? message 'insert-queue!) insert-queue!)
            ((eq? message 'delete-queue!) delete-queue!)
            ((eq? message 'peek) peek)
            ((eq? message 'empty?) empty-queue?)))
    dispatch))

; Stack Problems
(define (make-stack)
  (let ((top-ptr '()))
    
    (define (empty?) (null? top-ptr))
    (define (push! arg)
      (cond ((empty?) (set! top-ptr (list arg)))
            (else (let ((new-pair (cons arg top-ptr)))
                    (set! top-ptr new-pair)))))
    (define (pop!)
      (cond ((empty?) (error "Nothing in stack to pop!"))
            (else (let ((datum (peek)))
                    (set! top-ptr (cdr top-ptr))
                    datum))))
    (define (peek)
      (if (empty?) #f
          (car top-ptr)))

    (define (dispatch msg)
      (cond ((eq? msg 'peek) peek)
            ((eq? msg 'push!) push!)
            ((eq? msg 'pop!) pop!)))
    dispatch))
