; data types main definitions
(define (attach-type type contents)
  (cons type contents))

(define (type data)
  (car data))

(define (contents data)
  (cdr data))