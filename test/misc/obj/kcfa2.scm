(define g (lambda (obj) (set-car! obj 4)))

;(define g (lambda (obj) (define-data-property obj "test" 4)))
;(define g (lambda (obj) (define-accessor-property obj "test" (lambda (evt) 4) (lambda (evt) 4) )))
;(define g (lambda (obj) (add-event-listener obj "test" (lambda (evt) 4))))

(define f (lambda (x) (g x)))

(f (cons 1 2))
(f (cons 1 3))
