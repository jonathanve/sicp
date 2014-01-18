; Y Combinator
; Produces the fixed point of function f
; (Y F) = (F (Y F)) -> (F (F (F (F ... (F Io)))))
(define Y
	(lambda (f)
		((lambda (x) (x x))
		 (lambda (g)
		 	(f (lambda args (apply (g g) args)))))))

(define fib
  (Y
    (lambda (f)
      (lambda (x)
        (if (< x 2)
            x
            (+ (f (- x 1)) (f (- x 2))))))))

; test
(define lista (list 1 2 3 4 5 6 7 8 9 10))
(map fib lista)