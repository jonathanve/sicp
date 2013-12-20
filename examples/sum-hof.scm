; high order functions
; 	1. functions as parameters are allowed
;	2. functions can return other functions
; in summary -> awesome!

; sum(f(x), a, b, step)
(define (sum func a next b)
  (if (> a b)
      0
      (+ (func a)
	 (sum func (next a) next b))))

(define (sum-ints a b)
  (sum (lambda(x) x)
       a 
       (lambda(x) (+ x 1)) 
       b))

(define (sum-pi a b)
  (sum (lambda(x) (/ 1.0 (* x (+ x 2))))
       a 
       (lambda(x) (+ x 4)) 
       b))

; sum(x, 0, 10) = 55
(sum-ints 0 10)

; PI
(* 8 (sum-pi 1 1E5))

