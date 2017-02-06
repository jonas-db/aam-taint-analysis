;doesn't work because each event has equals one function call to f
; may with 0, must with 1evt/Cfa
(define window (object))
(define-data-property window 'a (taint #f))
(define-data-property window 'b 1)

(define f (lambda (x) (begin (sink x))))

(define listener (lambda (e) 
	(let ( (type (get-property e 'type)) )
		(if (equal? type 'a)
			(f (get-property window 'a))
			(f (get-property window 'b)))
		)))

(add-event-listener window 'a listener)
(add-event-listener window 'b listener)


(dispatch-event window (event 'b))
;(dispatch-event window (event 'b))
(emit window (event 'a))

(event-queue)