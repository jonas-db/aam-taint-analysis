(define a (object))
(define x 1)
(define-accessor-property a 'test (lambda () x) (lambda (s) (set! x s)))
(get-property a 'test)