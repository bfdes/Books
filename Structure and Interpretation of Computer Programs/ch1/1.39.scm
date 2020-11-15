(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ result (d i))))))
    (iter k 0))

(define (tan x k)
    (cont-frac
        (lambda (i) (if (= i 1) x (- (square x))))
        (lambda (i) (- (* 2 i) 1))
        k))
