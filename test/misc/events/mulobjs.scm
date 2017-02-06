(define g (lambda (obj) (add-event-listener obj "test" (lambda (evt) (display 'called)))))

(define f (lambda (x) (g x)))

(define a (object))
(define b (object))

(f a)
(f b)

(dispatch-event a "test" #f)

