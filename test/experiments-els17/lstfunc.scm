(define window (object))
(define f (lambda (x) (sink x)))

(add-event-listener window 'a (lambda (e) (f (taint #f))))
(add-event-listener window 'b (lambda (e) (begin (f 2)
	(emit window (event 'a)))))

(emit window (event 'b))
(event-queue)