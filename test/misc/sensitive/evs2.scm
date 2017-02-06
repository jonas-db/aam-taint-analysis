; example frm paper

(define obj (object))

(add-event-listener obj "a" (lambda (evt) (dispatch-event obj "b" #f)))
(add-event-listener obj "b" (lambda (evt) (display 1)))

(dispatch-event obj "a" #f)


(event-loop)

