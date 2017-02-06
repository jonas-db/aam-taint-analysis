(define window (object))

(define f (lambda (x) (sink x)))

(add-event-listener window 'a (lambda (e) (f (taint 1))))
(add-event-listener window 'b (lambda (e) (f 2)))

(event-loop)
