; abstract counting enabled -> correct

(define a (object))
(define b (object))

(define x (object))

; x is set to a, then to b, but they are both bound to o
(define (f o) (set! x o))

(define-data-property a "test" 4)

(define-data-property b "test" "ok")
(define-data-property b "test" #t)

(f a)
(f b)

(get-property x "test")
; right branch, is it because of it thinks he comes from (f a) and thus next step is (f b)?

;(define x 1)
;(define-accessor-property obj "test" (lambda () 1) (lambda (s) (set! x #t)))
;(define-accessor-property obj "test" (lambda () 1) (lambda (s) #f))


