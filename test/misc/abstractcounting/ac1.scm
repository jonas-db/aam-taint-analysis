; listenerstore: counting: true, strongremove: true

(define window (object))

(add-event-listener window "load" (lambda (event) 'load))
(remove-event-listener window "load" (lambda (event) 'load))

(add-event-listener window "load" (lambda (event) 'load2))

(event-loop)