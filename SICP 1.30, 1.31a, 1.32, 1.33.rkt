(define (identity x) x)
(define (1+ x) (+ x 1))
(define (square x) (* x x))


; SICP 1.30, define recursive sum with term
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; SICP 1.31(a), define recursive product with term
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; SICP 1.31(a), define factorial using product
(define (factorial a)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) a))

; SICP 1.31(a), approximate pi using product
(define (approx-pi accuracy) 
  (* 4 (product (lambda (x) (/ (* x (+ 2 x)) (* (+ x 1) (+ x 1)))) 2 (lambda (x) (+ x 2)) accuracy)))

; SICP 1.32, define accumulate with combiner
(define (accumulate combiner null-value term a next b)
  (define (iter a result) 
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; SICP 1.33, add filter to accumulate
(define (filtered-accumulate combiner null-value term filter a next b)
  (define (iter a result) 
    (cond 
      {(> a b) result}
      {(not (filter a)) (iter (next a) result)}
      {else (iter (next a) (combiner (term a) result))}))
  (iter a null-value))

; SICP 1.33(a), define sum-odds using filtered-accumulate
(define (sum-square-odds a b) (filtered-accumulate + 0 square odd? a 1+ b))

; SICP 1.33(b), define prod-relatively-prime using filtered-accumulate
(define (prod-relatively-prime n)
  (define (relatively-prime? a b factor)
    (cond 
      ((= factor (max a b))
       #t)
      ((and (= (remainder a factor) 0) (= (remainder n 
                                                     factor) 0))
       #f)
      (else 
       (relatively-prime? a b (+ factor 1)))))
  (filtered-accumulate * 1 identity (lambda (x) (relatively-prime? x n 2)) 1 1+ (- n 1)))