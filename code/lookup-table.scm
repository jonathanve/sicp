; Basic (type operation item) table implementation
; http://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on

(define *op-table* (make-hash-table))

(define (put type op proc)
  (hash-table/put! *op-table* (list type op) proc))

(define (get type op)
  (hash-table/get *op-table* (list type op) '()))