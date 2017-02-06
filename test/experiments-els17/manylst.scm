(define window (object))

(define l1 (lambda (e)
	(let ((o (object)))
		(sink (get-property o 'p))
		(set-property o 'p (taint 1)))))

(define l2 (lambda (e)
	(emit window (event 'c))))

(add-event-listener window 'a l1)
(add-event-listener window 'a l2)
(add-event-listener window 'c l1)

(emit window (event 'a))
(event-queue)