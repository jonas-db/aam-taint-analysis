(define obj (object))
(define key #f)

(add-event-listener obj 'keypress (lambda (e) (set! key (taint 1))))
(add-event-listener obj 'clear (lambda (e) (set! key #f)))
(add-event-listener obj 'unload (lambda (e) (sink key)))

(event-loop)
