; SICP 3.1a, the Cow class in scheme
(define (make-cow name weight)
  (define (get-name) name)
  (define (get-weight) weight)
  (define (eat grass)
    (set! weight (+ weight grass))
    weight)
  (define (speak) 'moo)
  (define (dispatch m)
    (cond ((eq? m 'get-name) get-name)
          ((eq? m 'get-weight) get-weight)
          ((eq? m 'eat) eat)
          ((eq? m 'speak) speak)
          (else (error "Unknown request -- MAKE-COW"
                       m))))
  dispatch)

; SICP 3.1b, test the Cow class
(define peppaCow (make-cow 'Peppa 200))
;((peppaCow 'get-name))
;((peppaCow 'get-weight))

(define georgeCow (make-cow 'George 250))
;((georgeCow 'eat) 50)
;((georgeCow 'speak))

; SICP 3.1c, create catCow which extends Cow
(define (catCow cow)
  (define (catSpeak) 'meow)
  (define (dispatch m)
    (cond ((eq? m 'speak) catSpeak)
          (else (cow m))))
  dispatch)

;(define zycat (catCow peppaCow))
;((zycat 'speak))