(define window (object))

(add-event-listener window 'load (lambda (event) (set-property window 'res "ok")))
(add-event-listener window 'load (lambda (event) (set-property window 'res 3)))

(dispatch-event window (event 'load))

(get-property window 'res)