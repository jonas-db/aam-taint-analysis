(define g (lambda (obj) (define-data-property obj "test" 4)))

(define f (lambda (x) (g x)))

(define a (object))
(define b (object))

(f a)
(f b)



