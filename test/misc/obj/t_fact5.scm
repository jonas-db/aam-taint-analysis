;0CFA: taint detected because obj is bound to a and b
;1CFA: taint detected
;(0CFA/1CFA) + object counting = not tainted
;

(define f (lambda (obj) (get-property obj "same")))

(define join (lambda (obj) (set-property obj "same" "ok")))

(define a (object))
(define-data-property a "id" (taint #t))

(define b (object))
(define-data-property b "id" 2)
(define-data-property b "same" (taint #t))
; redefining will join both values resulting is b[same] = {Clo, String}
(define-data-property b "same" "ok")

(f a)
(define res (f b))
(sink res)

