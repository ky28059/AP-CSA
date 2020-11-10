; SICP 2.1, create a better make-rat that supports negatives
(define (better-make-rat n d)
  (let ((g (gcd n d)))
    (cond {(< d 0) (cons (/ (* n -1) g) (/ (* d -1) g))}
          {else (cons (/ n g) (/ d g))})))

; SICP 2.2, abstraction for points and segments
; Define constructors make-segment and make-point and selectors start-segment, end-segment, x-point, y-point, to represent line segments in a plane
; Also make midpoint-segment which returns the midpoint of a list

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-line-segment start-point end-point)
  (cons start-point end-point))

(define (start-point line)
  (car line))

(define (end-point line)
  (cdr line))

(define (midpoint line)
  (make-point
   (/ (+ (x-point (start-point line)) (x-point (end-point line))) 2)
   (/ (+ (y-point (start-point line)) (y-point (end-point line))) 2)))

; SICP 2.4, refactor cdr to use functions
; This is commented out because casually changing the usage of such commonly used primitives as cons and cdr will generally mess up all your code
;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

; SICP 2.6, Alonzo Church lambda calculus
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f
       ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add n1 n2)
  (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))

; SICP 2.7, 2.2 but for intervals
(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

; SICP 2.8, create sub-interval
(define (sub-interval interval1 interval2)
  (make-interval
   (- (lower-bound interval1) (lower-bound interval2))
   (- (upper-bound interval1) (upper-bound interval2))))

; SICP 2.9, create width, prove that width of the sum of 2 intervals is the sum of the widths of the intervals being added
; This was a weirdly worded question and after much questioning Paley decided to eliminate it, so we only did the width part
(define (width interval)
  (abs (- (upper-bound interval) (lower-bound interval))))