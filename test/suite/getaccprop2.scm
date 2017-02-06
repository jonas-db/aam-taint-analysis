(define a (object))
(define x 1)
(get-property (define-accessor-property a 'test (lambda () x) (lambda (s) (set! x s))) 'test)
