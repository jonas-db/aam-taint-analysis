;FP - 0CFA: taint detected because obj is bound to a and b
;TP - 1CFA: no taint detected

(define f (lambda (obj) (get-property obj "id")))

(define a (object))
(define-data-property a "id" (taint #t))

(define b (object))
(define-data-property b "id" 2)

(f a)
(define res (f b))
(sink res)

