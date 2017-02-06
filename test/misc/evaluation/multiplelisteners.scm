;both sens/listeners because cfa like
;multiplelisteners
; nooooo event -> f call
(define window (object))
(define-accessor-property window 'url (lambda () (taint "EVIL")) (lambda (set) #f))

(define g (lambda (x) (begin (display x) (sink x))))
(define f (lambda (x) (g x)))


(add-event-listener window 'b (lambda (e) (f (get-property window 'url))))
(add-event-listener window 'b (lambda (e) (f (sanitize (get-property window 'url)))))

(emit window (event 'b))

(event-queue)