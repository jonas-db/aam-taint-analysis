(define a (object))
(define x 't)
(set! x 'a)

(define-data-property a x 1)

(get-property a x)