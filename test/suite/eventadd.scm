(define window (object))
(add-event-listener window 'load (lambda (event) (set-property window 'res "ok")))

(dispatch-event window (event 'load))

(get-property window 'res)

