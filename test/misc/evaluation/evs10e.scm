(define o (object))
(define key #f)

(add-event-listener o 'keypress (lambda (e)
    (let ()
        (emit o (event 'keypress))
        (emit o (event 'clear))
        (emit o (event 'save))
        (set! key (taint 1)))))

(add-event-listener o 'clear (lambda (e)
    (let ()
        (emit o (event 'keypress))
        (emit o (event 'clear))
        (set! key #f))))

(add-event-listener o 'save (lambda (e) (sink key)))

(emit o (event 'keypress))

(event-queue)
