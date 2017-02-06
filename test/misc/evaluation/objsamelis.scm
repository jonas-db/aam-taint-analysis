; listener is used for event 'a and 'b
; if we replace emit c by dispatch c, it still gives error because the context has changed
; due to that the event listener hasn't been executed completly, thus the context didn't  change yet
; 0 to be found

(define window (object))

(define l1 (lambda (e)
	(let ((o (object)))
		(sink (get-property o 'p))
		(set-property o 'p (taint 1)))))

(define l2 (lambda (e)
	(emit window (event 'c))))

(add-event-listener window 'a l1)
(add-event-listener window 'a l2)
(add-event-listener window 'c l1)

(emit window (event 'a))
(event-queue)