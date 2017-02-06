(define window (object))

(define x 0)

(define load_handler 
	(lambda (event) 
		(if (< x 3)
			(begin 
				(remove-event-listener window "load" #f)
				(dispatch-event window "click"))))) 

(add-event-listener window "load" load_handler)

(add-event-listener window "click" 
	(lambda (event) 
		(begin
			(add-event-listener window "load" load_handler)
			(set! x (+ x 1)))))

(event-loop)