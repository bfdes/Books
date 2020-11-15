(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (count-leaves tree)
    (accumulate + 0
        (map (lambda (t) (if (pair? t) (count-leaves t) 1)) tree))))
