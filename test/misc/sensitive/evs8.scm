(define window (object))
(define document (object))
(define button (object))
(define text-field (object))
(define-data-property text-field 'text "some_text")

(define-data-property window 'document document)
(define-data-property document 'button button)

(define counter 0)
(define click-listener (lambda (e) (if (> counter 2) (sink (get-property text-field 'text)))))

(add-event-listener window 'wload (lambda (e) 
	(add-event-listener document 'dload (lambda (e) 	
	
		(add-event-listener button 'click click-listener)
		(add-event-listener text-field 'input (lambda (e) (set-property text-field 'text (taint 1))))
	
	))))


(event-loop)
