(define obj (object))
(define key #f)
(define l (lambda (e) (set! key #f)))

(add-event-listener obj 'keypress (lambda (e) 
	(begin
		(add-event-listener obj 'clear l)
		(set! key (taint 1)))))

(add-event-listener obj 'unload (lambda (e) (sink key)))

(event-loop)
