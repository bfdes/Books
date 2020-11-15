(define (good-enough? guess prev-guess x)
  (< (/ (abs (- guess prev-guess)) x) 0.00001))

(define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cbrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (cbrt-iter (improve guess x) guess x)))

(define (cbrt x)
  (cbrt-iter (improve 1.0 x) 1.0 x))
