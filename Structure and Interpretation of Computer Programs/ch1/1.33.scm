(define (filtered-accumulate predicate? combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (if (predicate? a) (term a) null-value)
            (filtered-accumulate predicate? combiner null-value term (next a) next b))))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (prod n)
    (define (filter x) (= (gcd x n) 1))
    (define (identity x) x)
    (define (inc x) (+ x 1))
    (filtered-accumulate filter * 1 identity 1 inc n))
