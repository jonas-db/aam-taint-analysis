;joined
(define f (lambda () (object)))

(define window (f))

(add-event-listener window 'load (lambda (event) (set-property window 'res (taint "ok"))))
(add-event-listener window 'load (lambda (event) (set-property window 'res 3)))

(define joining (f))

(dispatch-event window (event 'load))

(define z (get-property window 'res))
(sink z)