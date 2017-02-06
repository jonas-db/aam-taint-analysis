;eventcallfunction
;call a function many times in an event listener will use the timeslots of kcfa very fast
; 0: tainted, 1CFA: tainted, 2CFA: not tainted <-> 1EVT: not tainted

(define obja #f)
(define objb #f)
(define window (object))

(define f (lambda (x) (sink x)))

(define (alloc-obj2) (object))
(define (alloc-obj) (alloc-obj2))

(add-event-listener window 'a (lambda (e)
	(let ((o (alloc-obj)))
		(set! obja o)
		(emit window (event 'b)))))

(add-event-listener window 'b (lambda (e)
	(let ((ox (alloc-obj)))
		(set! objb ox)
		(emit window (event 'c)))))

(add-event-listener window 'c (lambda (e)
	(begin  
		(set-property objb 'p (taint 1)) 
		(emit window (event 'd)))))	

(add-event-listener window 'd (lambda (e) (f (get-property obja 'p))))

(emit window (event 'a))
(event-queue)