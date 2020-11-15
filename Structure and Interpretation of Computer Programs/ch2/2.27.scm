(define (deep-reverse maybe-list)
    (if (null? maybe-list)
        maybe-list
        (append
            (deep-reverse (cdr maybe-list))
            (if (pair? (car maybe-list))
                (list (deep-reverse (car maybe-list)))
                (list (car maybe-list))))))
