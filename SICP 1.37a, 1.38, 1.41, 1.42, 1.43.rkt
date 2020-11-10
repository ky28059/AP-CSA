(define (identity x) x)

; SICP 1.37a, infinitely continuous fraction
(define (cont-frac n d k)
  (define (frac-helper counter)
    (cond
      {(= k counter) 0}
      {else (/ (n counter) (+ (d counter) (frac-helper (+ counter 1))))}))
  (frac-helper 1))

; SICP 1.38, approximate e using Euler's continuous fraction
(define (approximate-e accuracy)
  (define (d-term i)
    (cond {(= (remainder (- i 2) 3) 0) (+ (* 2 (/ (- i 2) 3)) 2)}
          {else 1}))
  (+ 2
     (cont-frac (lambda (i) 1.0)
                d-term
                accuracy)))

; SICP 1.41, double a procedure
(define (double procedure)
  (lambda (x) (procedure (procedure x))))

; SICP 1.42, define an implementation of f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

; SICP 1.43, redefine repeated
(define (repeated func num)
  (define (repeated-helper result iter)
    (cond
      {(= num iter) result}
      {else (repeated-helper 
             (lambda (x) (func (result x))) 
             (+ iter 1))}))
  (repeated-helper identity 0))