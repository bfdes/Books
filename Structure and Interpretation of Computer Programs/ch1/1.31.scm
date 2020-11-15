(define (prod term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (factorial n)
    (define (identity x) x)
    (define (inc x) (+ x 1))
    (prod identity 1 inc n))

(define (pi n)
    (define (term x)
        (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))
    (define (inc x) (+ x 1))
    (* 2.0 (prod term 1 inc n)))
