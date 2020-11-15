(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(define (reverse sequence)
    (fold-right (lambda (e res) (append res (list e))) '() sequence))

(define (reverse sequence)
    (fold-left (lambda (res e) (cons e res)) '() sequence))
