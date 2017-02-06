; removes wrong handler, event handler should not be removed

(define window (object))

(add-event-listener window "load" (lambda (event) 'load))
(remove-event-listener window "load" (lambda (event) 'load))

(dispatch-event window "load" #f)

(display "other case")

; removes correct handler, event handler should be removed

(define handler (lambda (event) 'load))
(add-event-listener window "load2" handler)

(remove-event-listener window "load2" handler)

(dispatch-event window "load2" #f)