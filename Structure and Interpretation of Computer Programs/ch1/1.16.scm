(define (exp b n)
    (define (exp-iter a b n)
        (cond 
                ((= n 0) a)
                ((even? n) (exp-iter a (square b) (/ n 2) ))
                (else (exp-iter (* a b) b (- n 1) ))))
    (exp-iter 1.0 b n))
