; utilities procedures

; newton-method(f(x,y)) where f(x,y) = x - y^2
(define (sqrt x)
  (newton
   (lambda(y) (- x (square y)))
   1.0))

; y(n+1) = yn - f(yn)/(df/dy)(yn)
(define (newton func guess)
  (define df (deriv func))
  (fixed-point
   (lambda(x) (- x (/ (func x) (df x))))
   guess))

; derivate (returns a function)
(define (deriv f)
  (define dx 1E-5)
  (lambda(x) (/ (- (f (+ x dx))
		   (f x))
		dx)))

; fixed point
(define (fixed-point func start)
  (define tolerance 0.0000001)
  (define (close-enough? old new)
    (< (/ (abs (- (func old) (func new))) (func old))
       tolerance))
  (define (iter old new)
    (if (close-enough? old new)
	new
	(iter new (func new))))
  (iter start (func start)))

; sum(f(x), a, b, step)
(define (sum func a next b)
  (if (> a b)
      0
      (+ (func a)
	 (sum func (next a) next b))))

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
