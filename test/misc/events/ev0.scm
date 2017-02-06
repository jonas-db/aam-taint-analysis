(define window (object))

(add-event-listener window "load" (lambda (event) 'load))
(add-event-listener window "unload" (lambda (event) 'unload))

(remove-event-listener window "load" (lambda (event) 'load))

(event-loop)