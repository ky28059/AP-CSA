(define (numeric-rank rank)
  (cond ((equal? rank 'a) 14)
        ((equal? rank 'k) 13)
        ((equal? rank 'q) 12)
        ((equal? rank 'j) 11)
        (else rank)))

(define (sort hand)
  ((repeated sort-once (- (count hand) 1)) hand))
(define (sort-once hand)
  (cond ((empty? hand) hand)
        ((= (count hand) 1) hand)
        ((> (numeric-rank (bf (first hand)))
            (numeric-rank (bf (first (bf hand)))))
         (se (first hand) (sort-once (bf hand))))
        (else (se (first (bf hand))
                  (sort-once (se (first hand) (bf (bf hand))))))))

(define (remove num sent)
  (cond [(empty? sent) '()]
        [(equal? num (bf (first sent))) (remove num (bf sent))]
        [else (se (first sent) (remove num (bf sent)))]))

(define (rip-off-every function sent)
  (if (empty? sent) '()
      (se (function (first sent)) (rip-off-every function (bf sent)))))

(define (to-words thing)
  (cond [(equal? thing 'd) 'Diamonds]
        [(equal? thing 'h) 'Hearts]
        [(equal? thing 'c) 'Clubs]
        [(equal? thing 's) 'Spades]
        [(equal? thing 'a) 'Aces]
        [(equal? thing 'k) 'Kings]
        [(equal? thing 'q) 'Queens]
        [(equal? thing 'j) 'Jacks]
        [(= thing 10) 'Tens]
        [(= thing 9) 'Nines]
        [(= thing 8) 'Eights]
        [(= thing 7) 'Sevens]
        [(= thing 6) 'Sixes]
        [(= thing 5) 'Fives]
        [(= thing 4) 'Fours]
        [(= thing 3) 'Threes]
        [(= thing 2) 'Twos]
        [else 'Ones]))

;---------------------------------------------------------------------------------------------------------------------------------------------------------

(define (pair? hand)
  (cond [(empty? hand) #f]
        [(member? (bf (first hand)) (rip-off-every bf (bf hand))) #t]
        [else (pair? (bf hand))]))
(define (pairof hand)
  (cond [(empty? hand) '(uh-oh...)]
        [(member? (bf (first hand)) (rip-off-every bf (bf hand))) (bf (first hand))]
        [else (pairof (bf hand))]))

(define (three-of-a-kind? hand)
  (cond [(empty? hand) #f]
        [(= (appearances (bf (first hand)) (rip-off-every bf hand)) 3) #t]
        [else (three-of-a-kind? (bf hand))]))
(define (threeof hand)
  (cond [(empty? hand) '(uh-oh...)]
        [(= (appearances (bf (first hand)) (rip-off-every bf hand)) 3) (bf (first hand))]
        [else (threeof (bf hand))]))

(define (four-of-a-kind? hand)
  (cond [(empty? hand) #f]
        [(= (appearances (bf (first hand)) (rip-off-every bf hand)) 4) #t]
        [else (four-of-a-kind? (bf hand))]))
(define (fourof hand)
  (cond [(empty? hand) '(uh-oh...)]
        [(= (appearances (bf (first hand)) (rip-off-every bf hand)) 4) (bf (first hand))]
        [else (fourof (bf hand))]))

(define (two-pair? hand)
  (if (and (pair? hand) (pair? (remove (pairof hand) hand))) #t #f))

(define (full-house? hand)
  (if (and (three-of-a-kind? hand) (pair? (remove (threeof hand) hand))) #t #f))

(define (straight? hand)
  (cond [(<= (count hand) 1) #t]
        [(= (- (numeric-rank (bf (first hand))) 1) (numeric-rank (bf (first (bf hand))))) (straight? (bf hand))]
        [else #f]))

(define (flush? hand)
  (cond [(<= (count hand) 1) #t]
        [(equal? (first (first hand)) (first (first (bf hand)))) (flush? (bf hand))]
        [else #f]))

(define (straight-flush? hand)
  (if (and (flush? hand) (straight? hand)) #t #f))
  
(define (royal-flush? hand)
  (cond [(equal? hand '(da dk dq dj d10)) #t]
        [(equal? hand '(ca ck cq cj c10)) #t]
        [(equal? hand '(ha hk hq hj h10)) #t]
        [(equal? hand '(sa sk sq sj s10)) #t]
        [else #f]))

;---------------------------------------------------------------------------------------------------------------------------------------------------------

(define (poker-value hand)
  (let ((sortedhand (sort hand)))
    (cond [(royal-flush? sortedhand)
           (se '(Royal flush -) (to-words (first (first hand))))]
          [(straight-flush? sortedhand)
           (se (se (to-words (bf (first hand))) '(high straight flush -)) (to-words (first (first hand))))]
          [(four-of-a-kind? sortedhand)
           (se '(Four of a kind -) (to-words (fourof hand)))]
          [(full-house? sortedhand)
           (se '(Full house -) (to-words (threeof hand)) 'over (to-words (pairof (remove (threeof hand) hand))))]
          [(flush? sortedhand)
           (se '(Flush -) (to-words (first (first hand))))]
          [(straight? sortedhand)
           (se (to-words (bf (first hand))) '(high straight))]
          [(three-of-a-kind? sortedhand)
           (se '(Three of a kind -) (to-words (threeof hand)))]
          [(two-pair? sortedhand)
           (se '(Two-pair -) (to-words (pairof hand)) 'and (to-words (pairof (remove (pairof hand) hand))))]
          [(pair? sortedhand)
           (se '(Pair of) (to-words (pairof hand)))]
          [else (se '(High hand -) (to-words (bf (first sortedhand))))])))

(poker-value '(ca ck cq cj c10))
;(Royal flush - Clubs)
(poker-value '(c10 c9 c8 c7 c6))
;(Tens high straight flush - Clubs)
(poker-value '(d3 d3 d3 c3 h6))
;(Four of a kind - Threes)
(poker-value '(h4 s4 c6 s6 c4))
;(Full house - Fours over Sixes)
(poker-value '(ca ck cj c9 c7))
;(Flush - Clubs)
(poker-value '(c9 h8 h7 h6 h5))
;(Nines high straight)
(poker-value '(da d6 d3 c3 h6))
;(Two-pair - Sixes and Threes)
(poker-value '(d3 d3 d3 c9 h6))
;(Three of a kind - Threes)
(poker-value '(da d3 d3 c9 h6))
;(Pair of Threes)
(poker-value '(dk d2 d3 c9 ha))
;(High hand - Aces)