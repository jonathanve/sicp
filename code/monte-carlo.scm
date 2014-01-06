(load "utils.scm")

; cesaro's function (experiment)
(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (monte-carlo trials experiment)
  (define (loop remaining passed)
    (cond ((= remaining 0)
	   (/ passed trials))
	  ((experiment)
	   (loop (-1+ remaining)
		 (1+ passed)))
	  (else
	   (loop (-1+ remaining)
		 passed))))
  (loop trials 0))

(define rand
  (let ((x (random 1.0)))
    (lambda()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ 
	     (floor (* a x)) b) m)))

; test
(average 4 5)

(estimate-pi 128)
(estimate-pi 256)
(estimate-pi 512)
(estimate-pi 1024)