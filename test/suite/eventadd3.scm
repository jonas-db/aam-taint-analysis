(define window (object))

(define l (lambda (event) (set-property window 'res "ok")))
(add-event-listener window 'load l)
(add-event-listener window 'load (lambda (event) (set-property window 'res 1)))

(remove-event-listener window 'load l)

(add-event-listener window 'load (lambda (event) (set-property window 'res #t)))

(dispatch-event window (event 'load))

(get-property window 'res)