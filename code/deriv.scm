; expressions (') quotations
(define (deriv exp var)
  (cond ((constant? exp var) 0)
	((same-var? exp var) 1)
	((sum? exp)
	 (make-sum (deriv (A1 exp) var)
		   (deriv (A2 exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (M1 exp)
			(deriv (M2 exp) var))
	  (make-product (M2 exp)
			(deriv (M1 exp) var))))))

; atom? -> cannot be divide it (minimum expressions)
(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

; a1 + 0 = a1
; 0 + a2 = a2
(define (make-sum a1 a2)
  (cond ((and (number? a1)
	      (number? a2))
	 (+ a1 a2))
	((and (number? a1) (= a1 0))
	 a2)
	((and (number? a2) (= a2 0))
	 a1)
	(else (list '+ a1 a2))))

; ax * 0 = 0
; ax * 1 = ax
(define (make-product m1 m2)
  (cond ((and (number? m1)
	      (number? m2))
	 (* m1 m2))
	((or (and (number? m1) (= m1 0))
	     (and (number? m2) (= m2 0)))
	 0)
	((and (number? m1) (= m1 1))
	 m2)
	((and (number? m2) (= m2 1))
	 m1)
	(else (list '* m1 m2))))

(define A1 cadr)
(define A2 caddr)
(define M1 cadr)
(define M2 caddr)

(define (atom? exp)
  (and (not (pair? exp))
       (not (null? exp))))

; use quotation(') to talk about expressions
(define f
  '(+ (* a (* x x))
      (+ (* b x)
	 c)))

(define g '(+ ( * (* x x) x) y))

f
g

; df/dx
(deriv f 'x)

; df/da
(deriv f 'a)

; df/db
(deriv f 'b)

; df/dc
(deriv f 'c)

; dg/dx
(deriv g 'x)

; dg/dy
(deriv g 'y)