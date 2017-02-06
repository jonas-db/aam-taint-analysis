(define window (object))
(define id (lambda (x) x))

(add-event-listener window 'a (lambda (e)
	(let ((t (id (taint 1))))
		(set-property window 'p (id #f))
		(add-event-listener window 'b (lambda (e) (sink (get-property window 'p))))
		(emit window (event 'b)))))

(emit window (event 'a))
(event-queue)