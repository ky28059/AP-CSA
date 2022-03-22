(define deck-of-cards
  '(sa sk sq sj s10 s9 s8 s7 s6 s5 s4 s3 s2
       ha hk hq hj h10 h9 h8 h7 h6 h5 h4 h3 h2
       ca ck cq cj c10 c9 c8 c7 c6 c5 c4 c3 c2
       da dk dq dj d10 d9 d8 d7 d6 d5 d4 d3 d2))


(define (bridge-val hand)
  (if (equal? (error-detection hand) '(Your hand is good to go.))
      (accumulate + (se 
                     (every (lambda (card)
                              (cond [(member? 'a card) 4]
                                    [(member? 'k card) 3]
                                    [(member? 'q card) 2]
                                    [(member? 'j card) 1]
                                    [else 0])) hand)
                     (every (lambda (number)
                              (cond [(equal? number 2) 1]
                                    [(equal? number 1) 2]
                                    [(equal? number 0) 3]
                                    [else 0]))
                            (se (count (keep (lambda (card) (member? 's card)) hand))                                           
                                (count (keep (lambda (card) (member? 'h card)) hand))                                         
                                (count (keep (lambda (card) (member? 'c card)) hand))                                         
                                (count (keep (lambda (card) (member? 'd card)) hand))))))
      (error-detection hand)))

(define (error-detection hand)
  (cond [(not (equal? (count hand) 13)) '(Your hand does not contain 13 cards!)]
        [(> (count (keep (lambda (card) (not (member? card deck-of-cards))) hand)) 0) '(Your hand contains an illegal card!)]
        [(not (equal? (accumulate + (every (lambda (card) (appearances card hand)) hand)) 13)) '(Your hand contains duplicate cards!)]
        [else '(Your hand is good to go.)]))


;(error-detection '(sa dk))
;(error-detection '(j3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
;(error-detection '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
;(error-detection '(h3 d7 sk s3 c10 dq dq s9 s4 d10 c7 d4 s2))

;(trace bridge-val)
;(trace error-detection)

(bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
(bridge-val '(sa dk))
(bridge-val '(j3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
(bridge-val '(h3 d7 sk s3 c10 dq dq s9 s4 d10 c7 d4 s2))


;(define (sort-hand hand)
;  (se
;   (every (lambda (card)
;            (cond [(member? 'a card) 14]
;                  [(member? 'k card) 13]
;                  [(member? 'q card) 12]
;                  [(member? 'j card) 11]
;                  [(equal? 0 (last card)) 10]
;                  [(equal? 9 (last card)) 9]
;                  [(equal? 8 (last card)) 8]
;                  [(equal? 7 (last card)) 7]
;                  [(equal? 6 (last card)) 6]
;                  [(equal? 5 (last card)) 5]
;                  [(equal? 4 (last card)) 4]
;                  [(equal? 3 (last card)) 3]
;                  [(equal? 2 (last card)) 2]))
;          (keep (lambda (card) (member? 's card)) hand))
;  (keep (lambda (card) (member? 'h card)) hand)
;  (keep (lambda (card) (member? 'c card)) hand)
;  (keep (lambda (card) (member? 'd card)) hand)))
;
;
;(sort-hand '(c4 ck hj s6 s2 hq s7 h9 s10 sa dk d9 d3))
