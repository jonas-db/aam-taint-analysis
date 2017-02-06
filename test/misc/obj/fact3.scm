(define f (lambda (obj) (define-data-property obj "id" 2)))

;(define f (lambda (x) (g x)))

(define a (object))
(define-data-property a "id" "a")
(define b (object))
(define-data-property b "id" "b")

(f a)
(f b)

(get-property b "id")
