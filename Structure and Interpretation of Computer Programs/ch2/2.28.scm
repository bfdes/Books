(define (fringe tree)
    (cond
        ((pair? tree) (append (fringe ( car tree)) (fringe (cdr tree))))
        ((null? tree) tree)
        (else (list tree))))
