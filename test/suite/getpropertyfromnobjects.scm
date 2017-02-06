(define o1 (object))

(define proto-a (object))
(define-data-property proto-a 'p "ok")
(set-prototype o1 proto-a)

(define o2 (object))

(define proto-b (object))
(define-data-property proto-b 'x "ok")
(set-prototype o2 proto-b)

; no join of objects, but x is bound to two objects
(define f (lambda (x) (get-property x 'p)))

(f o1)
(f o2)
; string from o1 'p ok and false from undefined on o2
