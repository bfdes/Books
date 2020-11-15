(define (double n)
    (* 2 n))

(define (halve n)
    (/ n 2))

(define (multiply a b)
    (define (multiply-iter a b c)
        (cond
            ((= c 0) a)
            ((even? c) (multiply-iter a (double b) (halve c)))
            (else (multiply-iter (+ a b) b (- c 1)))))
    (multiply-iter 0 a b))
