; Complex Arithmetic System

(load "types.scm")
(load "lookup-table.scm")

; Rectangular-based implementation

; 1. Constructors
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (make-rectangular-from-polar r A)
  (attach-type
   'rectangular
   (cons
    (* r (cos A))
    (* r (sin A)))))

; 2. Selectors
(define (real-part-rectangular z)
  (car z))

(define (imag-part-rectangular z)
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+
	 (square (real-part-rectangular z))
	 (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (/
	 (imag-part-rectangular z)
	 (real-part-rectangular z))))

; Polar-based implementation

; 1. Constructors

(define (make-polar r A)
  (attach-type 'polar (cons r A)))

(define (make-polar-from-rectangular x y)
  (attach-type
   'polar
   (cons
    (sqrt (+ (square x) (square y)))
    (atan (/ y x)))))

; 2. Selectors
(define (real-part-polar z)
  (* (car z) (cos (cdr z))))

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

; methods subscription
; type op-name procedure
(put 'rectangular 'real-part real-part-rectangular)
(put 'rectangular 'imag-part imag-part-rectangular)
(put 'rectangular 'magnitude magnitude-rectangular)
(put 'rectangular 'angle angle-rectangular)

(put 'polar 'real-part real-part-polar)
(put 'polar 'imag-part imag-part-polar)
(put 'polar 'magnitude magnitude-polar)
(put 'polar 'angle angle-polar)

; operate method (data-directed programming)
(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
	(proc (contents obj))
	(error "Undefined procedure"))))

; Selectors
(define (real-part obj)
  (operate 'real-part obj))

(define (imag-part obj)
  (operate 'imag-part obj))

(define (magnitude obj)
  (operate 'magnitude obj))

(define (angle obj)
  (operate 'angle obj))

; Operations
(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

; Tests

(define z1 (make-rectangular 4 3))
(define z2 (make-rectangular 3 4))
(define z3 (make-polar 5 1.5))
(define z4 (make-polar 5 0.5))

(real-part z1)
(real-part z3)
(imag-part z1)
(imag-part z3)

(magnitude z1)
(magnitude z3)
(angle z1)
(angle z3)

; rectangular
(+c z1 z2)
(-c z1 z2)
(*c z1 z2)
(/c z1 z2)

; polar
(+c z3 z4)
(-c z3 z4)
(*c z3 z4)
(/c z3 z4)