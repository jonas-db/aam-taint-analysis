(define objectA (object))
(define objectB (object))

; changing B to A resolves the problem..
(add-event-listener objectB "ev2" (lambda (evt) 'ax))
(add-event-listener objectA "ev1" (lambda (evt) 'bx))
(add-event-listener objectA "load" (lambda (evt) 'load))

(event-loop)