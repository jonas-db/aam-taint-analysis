(define g (lambda (obj) (dispatch-event obj "test" #f )))
;(define g (lambda (obj) (display obj)))
;(define g (lambda (obj) (set-car! obj 4)))
;(define g (lambda (obj) (define-data-property obj "test" 4)))
;(define g (lambda (obj) (define-accessor-property obj "test" (lambda (evt) 4) (lambda (evt) 4) )))
;(define g (lambda (obj) (add-event-listener obj "test" (lambda (evt) 4))))

(define f (lambda (x) (g x)))

(define a (object))
(add-event-listener a "test" (lambda (evt) (display 'a)))
(define b (object))
(add-event-listener b "test" (lambda (evt) (display 'b)))

(f a)
(f b)



