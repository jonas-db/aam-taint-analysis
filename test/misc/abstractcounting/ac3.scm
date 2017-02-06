; listenerstore: counting: true, strongremove: true

(define o (object))
(define-data-property o 'p (taint #t))

(define load-listener (lambda (event) (sink (get-property o 'p))))
(add-event-listener o 'load load-listener)
(remove-event-listener o 'load load-listener)

(dispatch-event o (event 'load))