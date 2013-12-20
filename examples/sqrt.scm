; sqrt(x) fixed point of function f(x,y) = x/y
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (/ (abs (- (square guess) x)) x)
       0.00001))
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (try 1.0))

; square definition
(define (square x)
  (* x x))

; absolute value definition
(define (abs x)
  (if (< x 0)
      (- x)
      x))

; average definition
(define (average x y)
  (/ (+ x y) 2))

; 11
(sqrt 121)

; Pythagorean triple (3,4,5)
(sqrt (+ (square 3) (square 4)))
