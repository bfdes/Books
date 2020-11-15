(define (equal? first second)
    (or
        (eq? first second)
        (and
            (and (pair? first) (pair? second))
            (and
                (equal? (car first) (car second))
                (equal? (cdr first) (cdr second))))))
