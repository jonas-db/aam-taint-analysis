(define window (object))
(define-data-property window 'url (taint "evil"))
(define data #f)

(define listener (lambda (e) (sink data)))

(add-event-listener window 'load (lambda (e)
	(letrec ()
		(add-event-listener window 'send listener) 
		(emit window (event 'send))
		(emit window (event 'unload)))))

(add-event-listener window 'unload (lambda (e)
	(letrec ()  
		(set! data (get-property window 'url))
		(dispatch-event window (event 'send)))))

(emit window (event 'load))
(event-queue)