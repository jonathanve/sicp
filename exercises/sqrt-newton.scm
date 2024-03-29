; high order functions
; 	1. functions as parameters are allowed
;	2. functions can return other functions
; in summary -> awesome!

; newton-method(f(x,y)) where f(x,y) = x - y^2
(define (sqrt x)
  (newton
   (lambda(y) (- x (square y)))
   1.0))

; y(n+1) = yn - f(yn)/(df/dy)(yn)
; fixed point (approximation old - new -> 0)
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

; f(f(f(f(f(...))))) ;-)
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

(define (abs x)
  (if (< x 0)
      (- x)
      x))

; sqrt(36) = 6
(sqrt 36)

; sqrt(1E10) = 316.227766017
(sqrt 1E5)

; sqrt(1E-10) = 1E-5 
(sqrt 1E-10)
