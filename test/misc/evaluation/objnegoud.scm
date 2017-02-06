;eventcallfunction
;call a function many times in an event listener will use the timeslots of kcfa very fast
; 0: tainted, 1CFA: not tainted, EVT always tainted because of second id function call

(define window (object))

(define id (lambda (x) x))

(add-event-listener window 'a (lambda (e)
	(let ((o (object))
		  (t (id (taint 1))))
		(set-property oa 'x (id #f))
		(add-event-listener window 'd (lambda (e) (sink (get-property obja 'x))))
		(emit window (event 'd)))))

(emit window (event 'a))
(event-queue)

(define obja #f)
(define objb #f)
(define window (object))

(define id (lambda (x) x))

(define f (lambda (x) (sink x)))
(add-event-listener window 'a (lambda (e)
	(let ((o (object))
		  (t (id (taint 1))))
		(set! obja o)
		(set-property obja 'x (id #f))
		(add-event-listener window 'd (lambda (e) (f (get-property obja 'x))))
		(emit window (event 'd)))))

;(add-event-listener window 'd (lambda (e) (f (get-property obja 'x))))

(emit window (event 'a))

(event-queue)