; segments
; points
; pairs

(load "utils.scm")

; ------------------------------
; segment level of abstraction
; a segment is a pair of points
(define (segment p q)
  (cons p q))

(define (start-point s)
  (car s))

(define (end-point s)
  (cdr s))

; note: let stablish a local context
(define (midpoint s)
  (let
      ((a (start-point s))
       (b (end-point s)))
    (point
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

(define (lenght s)
  (let
      ((dx (- (xcor (end-point s))
	      (xcor (start-point s))))
       (dy (- (ycor (end-point s))
	      (ycor (start-point s)))))
    (sqrt (+ (square dx)
	     (square dy)))))
; ------------------------------
; point level of abstraction
; A point is a pair of numbers
(define (point x y)
  (cons x y))

(define (xcor p)
  (car p))

(define (ycor p)
  (cdr p))
; ------------------------------

(define p (point 1 1))
(define q (point 4 4))
(define r (point -1 3))
(define v (point -10 -5))
(define s (segment p q))
(define c (segment r v))

; (2.5, 2.5)
(midpoint s)
; (-5.5,1)
(midpoint c)
; 8.732124598
(lenght (segment (midpoint s) (midpoint c)))
