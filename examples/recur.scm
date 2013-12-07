; fibonacci function (tail-recursive version)
(define (fib n)
  (define (loop a b n)
    (if (= n 0)
	a
	(loop b (+ a b) (- n 1))))
  (loop 0 1 n))

; tower of hanoi
(define (move n from to spare)
  (cond
  	((= n 0) "no moves")
	((= n 1) (print-move from to))
	(else
	  (move (- n 1) from spare to)
	  (move 1 from to spare)
	  (move (- n 1) spare to from))))

; print move helper definition
(define (print-move from to)
    (begin
      (newline)
      (display (list 'move 'from from 'to to))))

; 55
(fib 10)

; no moves
(move 0 1 2 3)

; just one move
(move 1 1 2 3)

; 31 moves (pow(2,n)-1)
(move 5 1 2 3)