(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (midpoint segment)
    (define (average start end)
        (/ (+ start end) 2))
    (cons
        (average (x-point (start-segment segment)) (x-point (end-segment segment)))
        (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
