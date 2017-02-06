(define f (lambda () (object)))

;a,b point to the same address
(define a (f))
(define-data-property a 't 1)

(define b (f))
(define-data-property b 'x 2)
(define-data-property b 'x 'no)

(delete-property a 't)
(get-property a 't)