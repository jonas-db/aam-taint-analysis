;results #f comes from the fact that the abstract interpreter doesn't know that all 'loads are from one object
; so it will try one config where there are no events trigger, which results in 'res of window being false (not defined)
(define window (object))

(define f (lambda () (object)))

(define a (f))

(define l (lambda (event) (begin (set-property window 'res 1) (display 'ev1))))
(add-event-listener a 'load l)
(add-event-listener a 'load (lambda (event) (set-property window 'res 'ev2)))

(define b (f))
(add-event-listener b 'load (lambda (event) (display 'ev3)))

(dispatch-event a (event 'load))


(get-property window 'res)