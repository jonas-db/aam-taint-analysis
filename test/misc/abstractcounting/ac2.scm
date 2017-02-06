; listenerstore: counting: true, strongremove: true


(define window (object))

(define (on-load event) 
	(begin
		(remove-event-listener window "load" on-load)
		(add-event-listener window "click" (lambda (event) 'click))))

(add-event-listener window "load" on-load)
(add-event-listener window "unload" (lambda (event) (remove-event-listener window "unload" #f)))


(event-loop)