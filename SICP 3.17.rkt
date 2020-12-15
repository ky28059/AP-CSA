; SICP 3.17, fix Bitdiddle's count pairs
(define (cp pair)
  (let ((visited '()))
    (define (visited? pair vl)
      (if (null? vl) #f
          (or (eq? pair (car vl)) (visited? pair (cdr vl)))))
    (define (cp2 pair)
      (cond ((not (pair? pair)) 0)
            ((visited? pair visited) 0)
            (else
             (set! visited (cons pair visited))
             (+ 1 (cp2 (car pair)) (cp2 (cdr pair))))))
    (cp2 pair)))

; This code returns 7 under Bitdiddle's function, but 3 under ours as expected
;(define deepest (list 3))
;(define lstlst (cons deepest deepest))
;(define z (cons lstlst lstlst))
;(cp z)
