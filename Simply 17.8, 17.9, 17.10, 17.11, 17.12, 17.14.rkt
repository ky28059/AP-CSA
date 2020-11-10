; Simply 17.8, redefine member
(define (member term list)
  (cond 
    {(empty? list) #f}
    {(equal? (car list) term) list}
    {else (member term (cdr list))}))

; Simply 17.9, redefine list-ref
(define (list-ref list item)
  (cond
    {(empty? list) #f}
    {(= item 0) (car list)}
    {else (list-ref (cdr list) (- item 1))}))

; Simply 17.10, redefine length
(define (length list)
  (if (empty? list)
      0
      (+ 1 (length (cdr list)))))

; Simply 17.11, write before-in-list
(define (before-in-list? list elem1 elem2)
  (let ((ref1 (member elem1 list)) (ref2 (member elem2 list)))
    (if (and ref1 ref2)
        (> (length ref1) (length ref2))
        #f)))

; Simply 17.12, write flatten
(define (flatten list)
  (cond
    {(empty? list) '()}
    {(word? (car list)) (cons (car list) (flatten (cdr list)))}
    {else (append (flatten (car list)) (flatten (cdr list)))}))

; Simply 17.14, define item but for nested lists
(define (branch index list)
  (cond
    {(empty? list) '()}
    {(empty? index) list}
    {else (branch (cdr index) (list-ref list (- (car index) 1)))}))