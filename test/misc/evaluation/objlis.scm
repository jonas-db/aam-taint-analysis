;eventcallfunction
; first a -> l1 
; then a -> l1 l2
;delete property on obja has no effect when obja is joined
; cfa never always false postiive
; 3evt wins
(define oa #f)
(define window (object))

(define listener2 (lambda (e) 
	(let ()
		(delete-property oa 'x)
		(remove-event-listener window 'a listener)
		(remove-event-listener window 'a listener2)
		(sink (get-property oa 'x)))))

(define listener (lambda (e)
	(let ((o (object)))
		(set! oa o)
		(set-property oa 'x (taint 1))		
		(add-event-listener window 'a listener2)				
		(emit window (event 'a)))))

(add-event-listener window 'a listener)

(emit window (event 'a))
(event-queue)