; high order functions
; 	1. functions as parameters are allowed
;	2. functions can return other functions
; in summary -> awesome!

; sqrt(x) = fixed-point(avg(x , x/y))
(define (sqrt x)
  (fixed-point
   (lambda(y) (average (/ x y) y))
   1.0))

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

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

; sqrt(36) = 6
(sqrt 36)

; sqrt(1E10) = 316.227766017
(sqrt 1E5)

; sqrt(1E-10) = 1E-5 
(sqrt 1E-10)
