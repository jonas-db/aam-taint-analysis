(define a (object))
(define-data-property a 'test 1)
(set-property a 'test "ok")
(get-property a 'test)