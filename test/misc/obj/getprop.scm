(define g (lambda (obj) (get-property obj "test")))

(define f (lambda (x) (g x)))

(define a (object))
(define-data-property a "test" #t)

(define b (object))
(define-data-property b "test" #f)

(f a)
(f b)