(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (let 
        ((sign (if (> d 0) + -))
        (g (gcd (abs n) (abs d))))
    (cons (sign (/ n g)) (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
    