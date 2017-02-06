(define window (object))
(define input-age (object))

(define (print message) (display message) (newline))

(define (input-listener event)
	(print "input listener called")
)

; load-listener enables input-name & input-age
(define (load-listener event)
	(begin
		(print "load listener called")
		(add-event-listener input-age "change" input-listener))
)

(add-event-listener window "load" load-listener)
(add-event-listener window "unload" (lambda (event) (begin (print "unload listener called") #f)))

(define unload-event (object))
(define-data-property unload-event "type" "load")
(define-data-property unload-event "some_data_property" "some_value")

;(dispatch-event window "load" unload-event)

(event-loop)