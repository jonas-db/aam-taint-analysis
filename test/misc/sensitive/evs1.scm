; taint will flow only when, keypress -> unload follow each other

(define obj (object))
(define o (object))

(define a (object))
(define-data-property a "lol" 2)

(define l (lambda (x) (set! o x)))

(set! o a)

(add-event-listener obj "keypress" (lambda (evt) (set-property o "test" (taint 1))))
(add-event-listener obj "unload" (lambda (evt) (sink (get-property o "test"))))

(event-loop)
