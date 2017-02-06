(define window (object))
(define x 1)
(define l (lambda (event) (set! x #f)))

(add-event-listener window 'load l)
(remove-event-listener window 'load l)

(dispatch-event window (event 'load))

x