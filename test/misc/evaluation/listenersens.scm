;both sens/listeners because cfa like
; doesn't work maps one function one event listenr
(define window (object))

(define f (lambda (x) (sink x)))
(define h (lambda (x) (f x)))
(define g (lambda (x) x))

(add-event-listener window 'b (lambda (e) (begin 
	(g (taint #f)) 
	(add-event-listener window 'a (lambda (e) (h (g 'a))))
	(emit window (event 'a)))))

(emit window (event 'b))

(event-queue)