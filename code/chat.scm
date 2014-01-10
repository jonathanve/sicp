; Now the order matters....
; Consequence of Assigment and the Environment Model
; This linear way of seeing programming is the reason why there are bad programmers.
; Instead of thinking in abstractions and developing using wishful thiking, they start
; by writing the implementation without thinking in the primitives and means of combination

; Here lives a simple chat!

; Primitive
; returns a dispatcher
(define (make-agent)
    ; local variables
    (let ((name 'name) (messages '()))
        ; set name local name
        (define (set-my-name! new-name)
            (cond
                ((eq? name new-name) 'done)
                (else
                    (set! name new-name)
                    'ok)))
        ; add action
        (define (add-message! message)
            (set! messages (cons message messages))
            'ok)
        ; message sending
        (define (send-a-message message to-agent)
            ((to-agent 'receive-message) message))
        ; inbox
        (define (message-received message)
            (newline)
            (display (list name 'received message))
            (add-message! message))
        ; dispatcher
        ; this is the interesting one! Analyzes the message and returns aprocedure
        (define (dispatch m)
            (cond
                ((eq? m 'get-name) name)
                ((eq? m 'get-messages) messages)
                ((eq? m 'set-name!)
                    set-my-name!)
                ((eq? m 'add-message!)
                    add-message!)
                ((eq? m 'send-message)
                    send-a-message)
                ((eq? m 'receive-message)
                    message-received)
                (else
                    (error "Wrong message" m))))
        dispatch))

; Primitives and ;eans of Combination
; Messages and dispatcher

(define julieth (make-agent))
(define israel (make-agent))
(define son (make-agent))

(define (get-name agent)
    (agent 'get-name))

(define (set-name! agent new-name)
    ((agent 'set-name!) new-name))

(define (send-message message from-agent to-agent)
    ((from-agent 'send-message) message to-agent))

(define (get-messages agent)
    (agent 'get-messages))

; Tests
(get-name julieth)
(set-name! julieth 'julieth)
(get-name julieth)

(get-name israel)
(set-name! israel 'israel)
(get-name israel)

(get-name son)
(set-name! son 'hijo)
(get-name son)

; Simple Chat (¡Bácano!)
(send-message 'hola-soy-julieta julieth israel)
(send-message 'hola-soy-israel israel julieth)
(send-message 'hola-hijo-tu-padre israel son)
(send-message 'hola-hijo-tu-madre julieth son)

; Inboxes
(get-messages israel)
(get-messages julieth)
(get-messages son)