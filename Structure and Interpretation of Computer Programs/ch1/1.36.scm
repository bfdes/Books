(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       0.00001))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-x)
    (fixed-point
        (lambda (x) (+ (/ x 2) (/ (log 1000) (* 2 (log x)))))
        2.0))
