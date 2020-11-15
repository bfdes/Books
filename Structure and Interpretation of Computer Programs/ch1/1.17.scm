(define (double n)
    (* 2 n))

(define (halve n)
    (/ n 2))

(define (multiply a b)
    (cond
        ((= b 0) 0)
        ((even? b) (multiply (double a) (halve b)))
        (else (+ a (multiply a (- b 1))))))
