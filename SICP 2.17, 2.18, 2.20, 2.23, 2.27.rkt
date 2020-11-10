; SICP 2.17, write last-pair
(define (last-pair list)
  ((repeated cdr (- (length list) 1)) list))

; SICP 2.18, reverse a list
(define (reverse lst)
  (cond
    {(empty? lst) '()}
    {else (append (reverse (cdr lst)) (list (car lst)))}))

; SICP 2.20, write same-parity
(define (same-parity n . rest)
  (append (list n)
          (filter (lambda (x) (equal? (remainder x 2) (remainder n 2)))
                  rest)))

; SICP 2.23, implement for-each
(define (for-each f lst)
  (cond
    {(empty? lst) #t}
    {else
     (f (car lst))
     (for-each f (cdr lst))}))

; SICP 2.27, deep reverse
(define (deep-reverse lst)
  (cond
    {(empty? lst) '()}
    {(list? (car lst)) (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst))))}
    {else (append (deep-reverse (cdr lst)) (list (car lst)))}))