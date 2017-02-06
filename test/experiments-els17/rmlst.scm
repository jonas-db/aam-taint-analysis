(define oa #f)
(define window (object))

(define (g) (object))
(define (f) (g))

(define listener (lambda (e) (sink (taint (get-property oa 'p)))))

(add-event-listener window 'a (lambda (e)
	(let ((o1 (f)))
		(set-property o1 'p (taint 1))
		(add-event-listener o1 'l listener)
		(set! oa o1)
		(emit window (event 'b)))))

(add-event-listener window 'b (lambda (e)
	(let ((unused (f)))
		(emit window (event 'c)))))

(add-event-listener window 'c (lambda (e)
	(let ()
		(remove-event-listener oa 'l listener)
		(dispatch-event oa (event 'l)))))

(emit window (event 'a))
(event-queue)