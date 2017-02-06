(define proto (object))
(define-data-property proto 'p "ok")

(define a (object))
(define-data-property a 'p 1)
(set-prototype a proto)

(get-property a 'p)