(define f (lambda () (object)))

; o1 and o2 point to the same address
(define o1 (f))
(define-data-property o1 'p 1)

(define o2 (f))


(get-property o2 'p)
