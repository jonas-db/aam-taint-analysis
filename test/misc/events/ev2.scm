(define window (object))
(define button (object))
(define-data-property button "_counter" 0)
(define input-name (object))
(define input-age (object))

(define unload-event (object))
(define-data-property unload-event "type" "unload")
(define-data-property unload-event "some_data_property" "some_value")

(define (click-listener event)	(dispatch-event window "unload" unload-event))

(define (input-listener event)
	(begin
		(set-property button "_counter" (+ (get-property button "_counter") 1))
		
		(if (== (get-property button "_counter") 2)

			(add-event-listener button "click" click-listener)
		)
	)
)

; load-listener enables input-name & input-age
(define (load-listener event)
	(begin
		(add-event-listener input-name "change" input-listener)
		(add-event-listener input-age "change" input-listener)
	)
)

; unload-listener marks the end of the sequence
(define (unload-listener event)
	#f ; terminates sequence - handled explicitly in the interpreter
)

(add-event-listener window "load" load-listener)
(add-event-listener window "unload" unload-listener)
