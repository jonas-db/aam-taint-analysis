(define proto-a (object))
(define-data-property proto-a 'p #t)

(define proto-b (object))
(define-data-property proto-b 'p 1)

(define f (lambda () (object)))

; o1 and o2 point to the same address
(define o1 (f))
(define o2 (f))

(set-prototype o1 proto-a)
(set-prototype o2 proto-b)

(get-property o2 'p)
