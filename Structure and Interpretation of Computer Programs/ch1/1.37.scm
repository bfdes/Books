(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ result (d i))))))
    (iter k 0))
