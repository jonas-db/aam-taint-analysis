(define obj (object))

;2 works, 3 not -> subsumtion was disabled and it didn't converge
(add-event-listener obj "keypress" (lambda (evt) #t))
(add-event-listener obj "clear" (lambda (evt) #f))
(add-event-listener obj "test" (lambda (evt) #f))

(event-loop)