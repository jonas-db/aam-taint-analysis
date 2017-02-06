;ZeroCfa: 97...
;OneCfa: Visited 3185 states in 160.390633091 seconds, 1 possible results: Set(Prod({#f, Int},error taint detected))


(define obj (object))
(define-data-property obj "test" 2)
(define-data-property obj "test" #f)
(get-property obj "test")

(define key #f)

(add-event-listener obj "callback" (lambda (evt) (set! key evt)))

(add-event-listener obj "keypress" (lambda (evt) (dispatch-event obj "callback" (taint 1))))
(add-event-listener obj "clear" (lambda (evt) (dispatch-event obj "callback" #f)))
(add-event-listener obj "unload" (lambda (evt) (sink key)))


(event-loop)
