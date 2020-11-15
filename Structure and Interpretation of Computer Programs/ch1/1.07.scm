(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess x)
  (< (/ (abs (- guess prev-guess)) x) 0.00001))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
  (sqrt-iter (improve 1.0 x) 1.0 x))
