;eventcallfunction
;call a function many times in an event listener will use the timeslots of kcfa very fast
; 0: tainted, 1CFA: not tainted, EVT always tainted because of second id function call

(define window (object))
(define id (lambda (x) x))

(add-event-listener window 'a (lambda (e)
	(let ((t (id (taint 1))))
		(set-property window 'p (id #f))
		(add-event-listener window 'b (lambda (e) (sink (get-property window 'p))))
		(emit window (event 'b)))))

(emit window (event 'a))
(event-queue)