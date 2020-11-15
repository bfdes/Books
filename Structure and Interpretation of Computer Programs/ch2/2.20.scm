(define (same-parity? first second)
    (= (% first 2) (% second 2))

(define (same-parity first . rest)
    (cond
        ((null? rest) rest)
        ((same-parity? first (car rest)) (cons first (same-parity rest)))
        (cons )))